{-# LANGUAGE TypeFamilies, TypeOperators, PolyKinds, DataKinds,
             NamedFieldPuns, ApplicativeDo, MultiParamTypeClasses #-}

module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Control.Exception
import Data.Foldable
import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe (mapMaybe)
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
import Control.Concurrent.Async (forConcurrently_)
import System.FilePath
import System.Directory
import System.IO.Error
import System.Exit (die)

data Config =
  Config {
    configHackagePath :: FilePath
  }

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
  downloadHackage config http_manager

downloadHackage :: Config -> HTTP.Manager -> IO ()
downloadHackage config http_manager = do
  all_package_ids <- requestPackages config http_manager
  missing_package_ids <- dropCachedPackages config all_package_ids
  let package_count = length missing_package_ids
  putStrLn ("Downloading " ++ show package_count ++ " packages")
  bad_package_ids_ref <- newIORef Set.empty
  forConcurrentlyInBuckets_ missing_package_ids $ \package_id -> do
    skipKnownFailures
      (atomicModifyIORef' bad_package_ids_ref (\acc -> (Set.insert package_id acc, ())))
      (downloadPackage config http_manager package_id)
  bad_package_ids <- readIORef bad_package_ids_ref
  let good_package_ids = Set.fromList all_package_ids `Set.difference` bad_package_ids
  updateBlacklist config bad_package_ids
  updateManifest config good_package_ids

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

requestPackages :: Config -> HTTP.Manager -> IO [Cabal.PackageId]
requestPackages config http_manager = do
  blacklist <-
    handleJust (\e -> if isDoesNotExistError e then Just Set.empty else Nothing) return $
    fmap (Set.fromList . mapMaybe Cabal.simpleParsec . lines) $
    readFile (configHackagePath config </> "blacklist")
  let blacklisted package_id = Set.member package_id blacklist
  tar_gz_req <-
    fmap HTTP.setRequestCheckStatus $
    HTTP.parseRequest "https://hackage.haskell.org/01-index.tar.gz"
  tar_gz_response <- HTTP.httpLbs tar_gz_req http_manager
  let tar_gz_content = HTTP.responseBody tar_gz_response
      tar_content = GZip.decompress tar_gz_content
  case getPackageIds tar_content of
    Nothing -> die "requestPackages: could not parse the index archive"
    Just package_ids -> return (filter (not . blacklisted) package_ids)

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

forConcurrentlyInBuckets_ :: [a] -> (a -> IO ()) -> IO ()
forConcurrentlyInBuckets_ items process_item = do
  num_capabilities <- getNumCapabilities
  let chunk_size = max 1 (length items `div` num_capabilities)
      buckets = List.chunksOf chunk_size items
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

downloadPackage :: Config -> HTTP.Manager -> Cabal.PackageId -> IO ()
downloadPackage config http_manager package_id = do
  withObjectPath config $ \package_object_path -> do
    package_files <- downloadPackageArchive http_manager package_id
    traverse_ (writePackageFile package_object_path) package_files
    createDirectoryIfMissing True (packagesPath config)
    POSIX.rename -- NB. atomic
      (package_object_path </> Cabal.prettyShow package_id)
      (packagePath config package_id)
