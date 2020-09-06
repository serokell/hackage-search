{-# LANGUAGE TypeFamilies, TypeOperators, PolyKinds, DataKinds,
             NamedFieldPuns, ApplicativeDo, MultiParamTypeClasses #-}

module Main (main) where

import System.IO
import System.IO.Error
import Control.Applicative
import Control.Concurrent (threadDelay)
import Servant
import Servant.Types.SourceT
import Network.Wai.Handler.Warp
import qualified Network.Socket
import qualified Data.Streaming.Network
import qualified Options.Applicative as Opt
import System.Process
import Control.DeepSeq (rnf)
import Control.Exception hiding (Handler)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Builder as ByteString.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import System.FilePath ((</>))

data Config =
  Config {
    configBindTarget :: BindTarget,
    configHackagePath :: FilePath,
    configFrontEndPath :: FilePath
  }

type HackageSearchAPI =
    "rg" :> Capture "pattern" String :> StreamGet NewlineFraming PlainText RgLineHandle
  :<|>
    Raw -- front-end

hackageSearchAPI :: Proxy HackageSearchAPI
hackageSearchAPI = Proxy

data RgLineHandle =
  RgLineHandle
    ProcessHandle
    Handle
    Handle

instance ToSourceIO Text RgLineHandle where
  toSourceIO (RgLineHandle p hOut hErr) = fromStepT go
    where
      go = Effect $ do
        eof <- hIsEOF hOut
        if eof then do
          err <- Text.hGetContents hErr
          if Text.null err
            then return stop
            else return (Yield (jsonEncodeErr err) stop)
        else do
          s <- Text.hGetLine hOut
          return $ Yield s go
      stop = Effect $ do
        terminateProcess p
        hClose hOut
        hClose hErr
        return Stop

hackageSearchServer :: Config -> Server HackageSearchAPI
hackageSearchServer config =
  searchH :<|> frontendH
  where
    searchH q = liftIO (rgSearch config q)
    frontendH = serveDirectoryFileServer (configFrontEndPath config)

packagesPath :: Config -> FilePath
packagesPath config = configHackagePath config </> "packages"

readManifest :: Config -> IO [String]
readManifest config = do
  s <-
    retryIf isDoesNotExistError (3, to_micro 1) $ -- See Note [Manifest update]
    System.IO.readFile (configHackagePath config </> "manifest")
  let package_ids = lines s
  evaluate (rnf package_ids)
  return package_ids

{- Note [Manifest update]
~~~~~~~~~~~~~~~~~~~~~~~~~
The downloader updates the manifest in three steps:
  1. create the new manifest
  2. delete the old manifest
  3. move the newly created manifest to the old path

Step 3 is atomic, so we are always guaranteed to read a complete, valid
manifest.  However, we may try to read in a time frame after step 2 and before
step 3, when the manifest is missing. The solution is to wait a little and try
to read it again.

It's nigh impossible that step 3 will take as long as several seconds, so we
give up aftewards and assume that the manifest is missing for another reason.
-}

to_micro :: Num a => a -> a
to_micro = (*) 1000000

retryIf :: Exception e => (e -> Bool) -> (Int, Int) -> IO a -> IO a
retryIf p (n, d) act = go n
  where
    go 0 = act
    go k =
      handleJust
        (\e -> if p e then Just () else Nothing)
        (\() -> do threadDelay d; go (k-1))
        act

rgSearch :: Config -> String -> IO RgLineHandle
rgSearch config rg_pattern = do
  package_ids <- readManifest config
  let rg_opts = ["--json", "--no-ignore", "--context", "2", "--regexp", rg_pattern]
  (_, Just hOut, Just hErr, p) <-
    createProcess ((proc "rg" (rg_opts ++ package_ids))
      { cwd = Just (packagesPath config),
        std_out = CreatePipe,
        std_err = CreatePipe,
        std_in = NoStream
      })
  return (RgLineHandle p hOut hErr)

jsonEncodeErr :: Text -> Text
jsonEncodeErr err = process enc
  where
    process =
      Text.decodeUtf8 .
      ByteString.Lazy.toStrict .
      ByteString.Builder.toLazyByteString .
      JSON.fromEncoding
    enc = JSON.pairs $
      Text.pack "type" JSON..= "error" <>
      Text.pack "message" JSON..= err

configOptP :: Opt.Parser Config
configOptP = do
  configBindTarget <-
    BindOnPort <$> Opt.option Opt.auto (Opt.long "port" <> Opt.metavar "NNNN")
      <|>
    BindOnUnixSocket <$> Opt.strOption (Opt.long "unix" <> Opt.metavar "PATH")
  configHackagePath <- Opt.strOption (Opt.long "hackage" <> Opt.metavar "PATH")
  configFrontEndPath <- Opt.strOption (Opt.long "frontend" <> Opt.metavar "PATH")
  pure Config{configBindTarget, configHackagePath, configFrontEndPath}

main :: IO ()
main = do
  config@Config{configBindTarget} <-
    Opt.execParser $
      Opt.info (configOptP <**> Opt.helper)
        (Opt.fullDesc <> Opt.header "Hackage Search")
  runServer configBindTarget (serve hackageSearchAPI (hackageSearchServer config))

data BindTarget
  = BindOnPort Int
  | BindOnUnixSocket FilePath

runServer :: BindTarget -> Application -> IO ()
runServer (BindOnPort port) = run port
runServer (BindOnUnixSocket path) = \app ->
  Network.Socket.withSocketsDo $
  bracket
    (Data.Streaming.Network.bindPath path)
    Network.Socket.close
    (\socket -> runSettingsSocket defaultSettings socket app)
