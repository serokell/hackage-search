{-# LANGUAGE NamedFieldPuns, ApplicativeDo #-}

module Main (main) where

import Prelude hiding (log)
import Control.Concurrent (getNumCapabilities, myThreadId)
import Control.Exception
import Data.Foldable
import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import qualified Options.Applicative as Opt
import qualified Data.List.Split as List
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Distribution.Package as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID (nextRandom)
import qualified System.Posix.Files as POSIX
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Control.Concurrent.Chan.Unagi as Chan
import qualified Data.Time.Clock as Time
import Control.Concurrent.Async (forConcurrently_, concurrently_)
import System.FilePath
import System.Directory
import System.IO.Error
import System.Exit (die)

data Config =
  Config {
    configHackagePath :: FilePath,
    configLimit :: Maybe Int
  }

configOptP :: Opt.Parser Config
configOptP = do
  configHackagePath <- Opt.strOption (Opt.long "hackage" <> Opt.metavar "PATH")
  configLimit <- optional (Opt.option Opt.auto (Opt.long "limit" <> Opt.metavar "LIMIT"))
  pure Config{configHackagePath, configLimit}

main :: IO ()
main = do
  config <- Opt.execParser $
    Opt.info (configOptP <**> Opt.helper)
      (Opt.fullDesc <> Opt.header "Hackage Download")
  http_manager <- HTTP.newTlsManager
  withLogger (downloadHackage config http_manager)

downloadHackage :: Config -> HTTP.Manager -> Logger -> IO ()
downloadHackage config http_manager logger = do
  all_package_ids <- requestPackages config http_manager logger
  missing_package_ids <- dropCachedPackages config all_package_ids
  log logger (LogMissingPackageCount (length missing_package_ids))
  let scheduled_package_ids = takeMaybe (configLimit config) missing_package_ids
  log logger (LogScheduledPackageCount (length scheduled_package_ids))
  bad_package_ids_ref <- newIORef Set.empty
  forConcurrentlyInBuckets_ logger scheduled_package_ids $ \package_id -> do
    skipKnownFailures
      (atomicModifyIORef' bad_package_ids_ref (\acc -> (Set.insert package_id acc, ())))
      (downloadPackage config http_manager logger package_id)
  bad_package_ids <- readIORef bad_package_ids_ref
  let good_package_ids = Set.fromList all_package_ids `Set.difference` bad_package_ids
  updateBlacklist config bad_package_ids
  updateManifest config good_package_ids

takeMaybe :: Maybe Int -> [a] -> [a]
takeMaybe Nothing = id
takeMaybe (Just k) = take k

updateBlacklist :: Config -> Set.Set Cabal.PackageId -> IO ()
updateBlacklist config package_ids =
  appendFile
    (configHackagePath config </> "blacklist")
    (unlines (map Cabal.prettyShow (Set.toList package_ids)))

updateManifest :: Config -> Set.Set Cabal.PackageId -> IO ()
updateManifest config package_ids = do
  withObjectPath config $ \manifest_object_path -> do
    let manifest_content = unlines (map Cabal.prettyShow (Set.toAscList package_ids))
        manifest_init_path = manifest_object_path </> "manifest"
        manifest_path = configHackagePath config </> "manifest"
    writeFile manifest_init_path manifest_content
    removePathForcibly manifest_path
    POSIX.rename -- NB. atomic
      manifest_init_path
      manifest_path

dropCachedPackages :: Config -> [Cabal.PackageId] -> IO [Cabal.PackageId]
dropCachedPackages config = filterOutM (doesPathExist . packagePath config)

packagesPath :: Config -> FilePath
packagesPath config = configHackagePath config </> "packages"

packagePath :: Config -> Cabal.PackageId -> FilePath
packagePath config package_id = packagesPath config </> Cabal.prettyShow package_id

filterOutM :: Applicative m => (a -> m Bool) -> [a] -> m [a]
filterOutM f = filterM (fmap not . f)

skipKnownFailures :: IO () -> IO () -> IO ()
skipKnownFailures on_fail =
    handleLegalReasons . handle410
  where
    handleLegalReasons = handleJust isLegalReasons (\_ -> on_fail)
    handle410 = handleJust is410 (\_ -> on_fail)

    is410 (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException r _))
      | HTTP.statusCode (HTTP.responseStatus r) == 410
      = Just ()
    is410 _ = Nothing

    isLegalReasons (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException r s))
      | HTTP.statusCode (HTTP.responseStatus r) == 451
      , ByteString.Char8.pack "Unavailable For Legal Reasons" `ByteString.isInfixOf` s
      = Just ()
    isLegalReasons _ = Nothing

requestPackages :: Config -> HTTP.Manager -> Logger -> IO [Cabal.PackageId]
requestPackages config http_manager logger = do
  tar_gz_req <-
    fmap HTTP.setRequestCheckStatus $
    HTTP.parseRequest "https://hackage.haskell.org/01-index.tar.gz"
  log logger LogIndexDownload
  tar_gz_response <- HTTP.httpLbs tar_gz_req http_manager
  let tar_gz_content = HTTP.responseBody tar_gz_response
      tar_content = GZip.decompress tar_gz_content
  case getPackageIds tar_content of
    Nothing -> die "requestPackages: could not parse the index archive"
    Just package_ids -> do
      log logger (LogIndexedPackageCount (length package_ids))
      nonblacklisted_package_ids <- removeBlacklisted config logger package_ids
      log logger (LogIndexedNonBlacklistedPackageCount (length nonblacklisted_package_ids))
      return nonblacklisted_package_ids

removeBlacklisted :: Config -> Logger -> [Cabal.PackageId] -> IO [Cabal.PackageId]
removeBlacklisted config logger package_ids = do
  blacklist <-
    handleJust (\e -> if isDoesNotExistError e then Just Set.empty else Nothing) return $
    fmap (Set.fromList . mapMaybe Cabal.simpleParsec . lines) $
    readFile (configHackagePath config </> "blacklist")
  log logger (LogBlacklistedPackageCount (Set.size blacklist))
  let blacklisted package_id = Set.member package_id blacklist
  return $ filter (not . blacklisted) package_ids

getPackageIds :: ByteString.Lazy.ByteString -> Maybe [Cabal.PackageId]
getPackageIds =
    either (const Nothing) (Just . Map.elems) .
    Tar.foldlEntries f Map.empty . Tar.read
  where
    f package_ids e =
      case get_cabal_file e of
        Nothing -> package_ids
        Just gen_pkg_desc ->
          let package_id = Cabal.packageId gen_pkg_desc
          in Map.insertWith (max_on Cabal.pkgVersion)
              (Cabal.pkgName package_id) package_id package_ids
    get_cabal_file e
      | [pkg_name_str, pkg_ver_str, cabal_file_name] <- splitDirectories (Tar.entryPath e),
        takeExtension cabal_file_name == ".cabal",
        Just pkg_name <- Cabal.simpleParsec pkg_name_str,
        Just pkg_ver <- Cabal.simpleParsec pkg_ver_str
      = Just (Cabal.PackageIdentifier pkg_name pkg_ver)
      | otherwise = Nothing

max_on :: Ord b => (a -> b) -> a -> a -> a
max_on f a b
  | f a > f b = a
  | otherwise = b

forConcurrentlyInBuckets_ :: Logger -> [a] -> (a -> IO ()) -> IO ()
forConcurrentlyInBuckets_ logger items process_item = do
  num_capabilities <- getNumCapabilities
  let chunk_size = max 1 (length items `div` num_capabilities)
      buckets = List.chunksOf chunk_size items
  log logger (LogThreadCount num_capabilities (length buckets))
  forConcurrently_ buckets (traverse_ process_item)

data PackageFile =
  PackageFile {
    packageFilePath :: FilePath,
    packageFileContent :: ByteString.Lazy.ByteString
  }

writePackageFile :: FilePath -> PackageFile -> IO ()
writePackageFile base_dir package_file = do
  let abs_dir_path  = base_dir </> takeDirectory (packageFilePath package_file)
      abs_file_path = base_dir </> packageFilePath package_file
  createDirectoryIfMissing True abs_dir_path
  ByteString.Lazy.writeFile abs_file_path (packageFileContent package_file)

downloadPackageArchive :: HTTP.Manager -> Cabal.PackageId -> IO [PackageFile]
downloadPackageArchive http_manager package_id = do
  let package_name_str = Cabal.unPackageName (Cabal.pkgName package_id)
      package_id_str = Cabal.prettyShow package_id
  tar_gz_req <-
    fmap HTTP.setRequestCheckStatus $
    HTTP.parseRequest $
      "https://hackage.haskell.org/package/" ++
        package_name_str ++ "/" ++ package_id_str ++ ".tar.gz"
  tar_gz_response <- HTTP.httpLbs tar_gz_req http_manager
  let tar_gz_content = HTTP.responseBody tar_gz_response
      tar_content = GZip.decompress tar_gz_content
  case getPackageFiles tar_content of
    Nothing -> die ("downloadPackageArchive: could not parse the archive for " ++ package_name_str)
    Just entries -> return entries

getPackageFiles :: ByteString.Lazy.ByteString -> Maybe [PackageFile]
getPackageFiles =
    either (const Nothing) Just . Tar.foldlEntries f [] . Tar.read
  where
    f package_files e =
      case get_package_file e of
        Nothing -> package_files
        Just package_file -> package_file : package_files
    get_package_file e
      | Tar.NormalFile content _ <- Tar.entryContent e =
          Just (PackageFile (Tar.entryPath e) content)
      | otherwise = Nothing

withObjectPath :: Config -> (FilePath -> IO a) -> IO a
withObjectPath config =
    bracket allocate_object_path removePathForcibly
  where
    allocate_object_path = do
      uuid_str <- fmap UUID.toString UUID.nextRandom
      let object_path = configHackagePath config </> "objects" </> uuid_str
      createDirectoryIfMissing True object_path
      return object_path

downloadPackage :: Config -> HTTP.Manager -> Logger -> Cabal.PackageId -> IO ()
downloadPackage config http_manager logger package_id = do
  withObjectPath config $ \package_object_path -> do
    log logger (LogPackageDownloadStart package_id)
    package_files <- downloadPackageArchive http_manager package_id
    traverse_ (writePackageFile package_object_path) package_files
    createDirectoryIfMissing True (packagesPath config)
    POSIX.rename -- NB. atomic
      (package_object_path </> Cabal.prettyShow package_id)
      (packagePath config package_id)
    log logger (LogPackageDownloadEnd package_id)

newtype Logger = Logger { log :: LogMessage -> IO () }

data LogAction = LogPut String | EndLogging

withLogger :: (Logger -> IO ()) -> IO ()
withLogger cont = do
  (inChan, outChan) <- Chan.newChan
  let logger = mkLogger inChan
      worker =
        Chan.readChan outChan >>= \m_msg ->
        case m_msg of
          EndLogging -> return ()
          LogPut msg -> putStrLn msg >> worker
      action = cont logger
        `finally` Chan.writeChan inChan EndLogging
  concurrently_ worker action

mkLogger :: Chan.InChan LogAction -> Logger
mkLogger chan =
  Logger $ \message -> do
    thread_id <- myThreadId
    timestamp <- Time.getCurrentTime
    msgstr <- evaluate $
      "[" ++ show timestamp ++ "] " ++
      "[" ++ show thread_id ++ "] " ++
      renderLogMessage message
    Chan.writeChan chan (LogPut msgstr)

data LogMessage =
    LogIndexDownload
  | LogIndexedPackageCount !Int
  | LogBlacklistedPackageCount !Int
  | LogIndexedNonBlacklistedPackageCount !Int
  | LogMissingPackageCount !Int
  | LogScheduledPackageCount !Int
  | LogThreadCount !Int !Int
  | LogPackageDownloadStart Cabal.PackageId
  | LogPackageDownloadEnd Cabal.PackageId

renderLogMessage :: LogMessage -> String
renderLogMessage LogIndexDownload =
  "Downloading package index"
renderLogMessage (LogIndexedPackageCount n) =
  "Indexed packages: " ++ show n
renderLogMessage (LogBlacklistedPackageCount n) =
  "Blacklisted packages: " ++ show n
renderLogMessage (LogIndexedNonBlacklistedPackageCount n) =
  "Indexed packages not in the blacklist: " ++ show n
renderLogMessage (LogMissingPackageCount n) =
  "Missing packages (download required): " ++ show n
renderLogMessage (LogScheduledPackageCount n) =
  "Scheduled for download packages: " ++ show n
renderLogMessage (LogThreadCount ncaps buckets) =
  "Using " ++ show buckets ++ " threads out of " ++
    show ncaps ++ " available (use +RTS -N to specify)"
renderLogMessage (LogPackageDownloadStart package_id) =
  "Starting to download " ++ Cabal.prettyShow package_id
renderLogMessage (LogPackageDownloadEnd package_id) =
  "Done with " ++ Cabal.prettyShow package_id
