{-# LANGUAGE TypeFamilies, TypeOperators, PolyKinds, DataKinds,
             NamedFieldPuns, ApplicativeDo, MultiParamTypeClasses #-}

module Main where

import Prelude hiding (log)
import Control.Concurrent (myThreadId, getNumCapabilities)
import Control.Exception
import Data.Text (Text)
import Data.Foldable
import Data.IORef
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
import qualified Data.Time.Clock.System as Time
import qualified Data.Time.Clock as Time
import qualified Data.Time.Format as Time
import Data.String (fromString)
import Control.Concurrent.Async (forConcurrently_)
import System.FilePath (takeDirectory, (</>))
import System.Directory (createDirectoryIfMissing, removeFile)
import System.Exit (die)

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

dup :: a -> (a, a)
dup x = (x, x)

estimate :: Double -> Time.SystemTime -> Time.SystemTime -> String
estimate progress_fraction time_of_start time_now = showSeconds seconds_remaining
  where
    showSeconds x = Time.formatTime Time.defaultTimeLocale "%mm" (Time.secondsToDiffTime x)
    seconds_remaining = ceiling (seconds_total - seconds_passed)
    seconds_total = seconds_passed / progress_fraction
    seconds_passed = fromIntegral $
      Time.systemSeconds time_now -
      Time.systemSeconds time_of_start

main :: IO ()
main = do
  http_manager <- HTTP.newTlsManager
  bracket
    (Log.newStdoutLoggerSet Log.defaultBufSize)
    Log.flushLogStr
    (\logger -> downloadHackage logger http_manager)

downloadHackage :: Log.LoggerSet -> HTTP.Manager -> IO ()
downloadHackage logger http_manager = do
  num_capabilities <- getNumCapabilities
  package_names <- requestPackages http_manager
  let package_count = length package_names
  log logger ("Downloading " ++ show package_count ++ " packages using " ++ show num_capabilities ++ " threads")
  counter <- newIORef (0 :: Int)
  time_of_start <- Time.getSystemTime
  let package_name_buckets = List.chunksOf (package_count `div` num_capabilities) package_names
  forConcurrently_ package_name_buckets $ \package_name_bucket ->
    for_ package_name_bucket $ \package_name -> do
      let package_name_str = packageNameStr package_name
      log logger ("Processing package: " ++ package_name_str)
      skipKnownFailures logger $ downloadPackage logger http_manager package_name
      n <- atomicModifyIORef counter (\n -> dup (n+1))
      time_now <- Time.getSystemTime
      let progress_fraction = fromIntegral n / fromIntegral package_count
      log logger $
        "Progress: " ++ show n ++ "/" ++ show package_count ++
          " (ETA: " ++ estimate progress_fraction time_of_start time_now ++ ")"

skipKnownFailures :: Log.LoggerSet -> IO () -> IO ()
skipKnownFailures logger =
    handleLegalReasons . handle410
  where
    handleLegalReasons = handleJust isLegalReasons $ \_ ->
      log logger "Skipping package unavailable for legal reasons"
    handle410 = handleJust is410 $ \_ ->
      log logger "Skipping due to 410"

    is410 (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException r _))
      | HTTP.statusCode (HTTP.responseStatus r) == 410
      = Just ()
    is410 _ = Nothing

    isLegalReasons (HTTP.HttpExceptionRequest _ (HTTP.StatusCodeException r s))
      | HTTP.statusCode (HTTP.responseStatus r) == 451
      , ByteString.Char8.pack "Unavailable For Legal Reasons" `ByteString.isInfixOf` s
      = Just ()
    isLegalReasons _ = Nothing

requestPackages :: HTTP.Manager -> IO [PackageName]
requestPackages http_manager = do
  init_req <-
    fmap HTTP.setRequestCheckStatus $
    HTTP.parseRequest "https://hackage.haskell.org/packages/"
  let req = init_req { HTTP.requestHeaders = [header_accept_json] }
  response <- HTTP.httpLbs req http_manager
  case JSON.decode (HTTP.responseBody response) of
    Nothing -> die "requestPackages: could not parse the response"
    Just r -> return r

identifyPackage :: HTTP.Manager -> PackageName -> IO Cabal.PackageId
identifyPackage http_manager package_name = do
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
  let package_id_str = Cabal.prettyShow package_id
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

downloadPackage :: Log.LoggerSet -> HTTP.Manager -> PackageName -> IO ()
downloadPackage logger http_manager package_name = do
  let package_name_str = packageNameStr package_name
  package_id <- identifyPackage http_manager package_name
  let package_id_str = Cabal.prettyShow package_id
  log logger ("Constructed package_id: " ++ package_id_str)
  package_files <- downloadPackageArchive http_manager package_id
  traverse_ (writePackageFile ".") package_files

header_accept_json :: HTTP.Header
header_accept_json = (HTTP.hAccept, ByteString.Char8.pack "application/json")
