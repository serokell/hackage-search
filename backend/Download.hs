{-# LANGUAGE TypeFamilies, TypeOperators, PolyKinds, DataKinds,
             NamedFieldPuns, ApplicativeDo, MultiParamTypeClasses #-}

module Main (main) where

import Prelude hiding (log)
import Control.Concurrent (myThreadId, getNumCapabilities)
import Control.Exception
import Data.Text (Text)
import Data.Foldable
import Data.IORef
import Control.Applicative
import Control.Monad
import qualified Options.Applicative as Opt
import qualified Data.List.Split as List
import qualified Data.Text as Text
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Distribution.Package as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Archive.Tar as Tar
import qualified System.Log.FastLogger as Log
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID (nextRandom)
import qualified System.Posix.Files as POSIX
import qualified Data.List as List
import Data.String (fromString)
import Control.Concurrent.Async (forConcurrently_)
import System.FilePath (takeDirectory, (</>))
import System.Directory
import System.Exit (die)

data Config =
  Config {
    configHackagePath :: FilePath
  }

newtype PackageName = PackageName { packageNameText :: Text }

packageNameStr :: PackageName -> String
packageNameStr = Text.unpack . packageNameText

instance JSON.FromJSON PackageName where
  parseJSON (JSON.Object v) = do
    name <- v JSON..: Text.pack "packageName"
    return (PackageName name)
  parseJSON invalid =
    JSON.prependFailure "parsing PackageName failed, " $
    JSON.typeMismatch "Object" invalid

log :: Log.LoggerSet -> String -> IO ()
log logger s = do
  thread_id <- myThreadId
  Log.pushLogStr logger (fromString ("[" ++ show thread_id ++ "] " ++ s ++ "\n"))

configOptP :: Opt.Parser Config
configOptP = do
  configHackagePath <- Opt.strOption (Opt.long "hackage" <> Opt.metavar "PATH")
  pure Config{configHackagePath}

main :: IO ()
main = do
  config <- Opt.execParser $
    Opt.info (configOptP <**> Opt.helper)
      (Opt.fullDesc <> Opt.header "Hackage Download")
  http_manager <- HTTP.newTlsManager
  bracket
    (Log.newStdoutLoggerSet Log.defaultBufSize)
    Log.flushLogStr
    (\logger -> downloadHackage config logger http_manager)

downloadHackage :: Config -> Log.LoggerSet -> HTTP.Manager -> IO ()
downloadHackage config logger http_manager = do
  all_package_ids <- requestPackages logger http_manager
  missing_package_ids <- dropCachedPackages config all_package_ids
  let package_count = length missing_package_ids
  log logger ("Downloading " ++ show package_count ++ " packages")
  forConcurrentlyInBuckets_ missing_package_ids (downloadPackage config http_manager)
  updateManifest config all_package_ids

updateManifest :: Config -> [Cabal.PackageId] -> IO ()
updateManifest config package_ids = do
  withObjectPath config $ \manifest_object_path -> do
    let manifest_content = unlines (List.sort (map Cabal.prettyShow package_ids))
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

skipKnownFailures :: Log.LoggerSet -> PackageName -> IO a -> IO (Maybe a)
skipKnownFailures logger package_name =
    handleLegalReasons . handle410 . fmap Just
  where
    handleLegalReasons = handleJust isLegalReasons $ \_ -> do
      log logger ("Skipping " ++ package_name_str ++ ": unavailable for legal reasons")
      return Nothing
    handle410 = handleJust is410 $ \_ -> do
      log logger ("Skipping " ++ package_name_str ++ ": 410 Gone")
      return Nothing
    package_name_str = packageNameStr package_name

    is410 (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException r _))
      | HTTP.statusCode (HTTP.responseStatus r) == 410
      = Just ()
    is410 _ = Nothing

    isLegalReasons (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException r s))
      | HTTP.statusCode (HTTP.responseStatus r) == 451
      , ByteString.Char8.pack "Unavailable For Legal Reasons" `ByteString.isInfixOf` s
      = Just ()
    isLegalReasons _ = Nothing

requestPackages :: Log.LoggerSet -> HTTP.Manager -> IO [Cabal.PackageId]
requestPackages logger http_manager = do
  log logger "Requesting packages"
  init_req <-
    fmap HTTP.setRequestCheckStatus $
    HTTP.parseRequest "https://hackage.haskell.org/packages/"
  let req = init_req { HTTP.requestHeaders = [header_accept_json] }
  response <- HTTP.httpLbs req http_manager
  package_names <-
    case JSON.decode (HTTP.responseBody response) of
      Nothing -> die "requestPackages: could not parse the response"
      Just r -> return r
  forConcurrentlyMaybe package_names (identifyPackage logger http_manager)

forConcurrentlyMaybe :: [a] -> (a -> IO (Maybe b)) -> IO [b]
forConcurrentlyMaybe items f = do
  results_ref <- newIORef []
  forConcurrentlyInBuckets_ items $ \item -> do
    m_result <- f item
    for_ m_result $ \result -> do
      atomicModifyIORef' results_ref (\results -> (result:results, ()))
  readIORef results_ref

forConcurrentlyInBuckets_ :: [a] -> (a -> IO ()) -> IO ()
forConcurrentlyInBuckets_ items process_item = do
  num_capabilities <- getNumCapabilities
  let chunk_size = max 1 (length items `div` num_capabilities)
      buckets = List.chunksOf chunk_size items
  forConcurrently_ buckets (traverse_ process_item)

identifyPackage :: Log.LoggerSet -> HTTP.Manager -> PackageName -> IO (Maybe Cabal.PackageId)
identifyPackage logger http_manager package_name =
  skipKnownFailures logger package_name $ do
    let package_name_str = packageNameStr package_name
    cabal_file_req <-
      fmap HTTP.setRequestCheckStatus $
      HTTP.parseRequest $
        "https://hackage.haskell.org/package/" ++
          package_name_str ++ "/" ++ package_name_str ++ ".cabal"
    cabal_file_response <- HTTP.httpLbs cabal_file_req http_manager
    let cabal_file_contents = ByteString.Lazy.toStrict (HTTP.responseBody cabal_file_response)
    package_id <-
      case Cabal.parseGenericPackageDescriptionMaybe cabal_file_contents of
        Nothing -> die ("downloadPackage: could not parse the cabal file for " ++ package_name_str)
        Just gen_pkg_desc -> return (Cabal.packageId gen_pkg_desc)
    let package_id_str = Cabal.prettyShow package_id
    log logger ("Identified: " ++ package_id_str)
    return package_id

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

downloadPackage :: Config -> HTTP.Manager -> Cabal.PackageId -> IO ()
downloadPackage config http_manager package_id = do
  withObjectPath config $ \package_object_path -> do
    package_files <- downloadPackageArchive http_manager package_id
    traverse_ (writePackageFile package_object_path) package_files
    createDirectoryIfMissing True (packagesPath config)
    POSIX.rename -- NB. atomic
      (package_object_path </> Cabal.prettyShow package_id)
      (packagePath config package_id)

header_accept_json :: HTTP.Header
header_accept_json = (HTTP.hAccept, ByteString.Char8.pack "application/json")
