{-# LANGUAGE TypeFamilies, TypeOperators, PolyKinds, DataKinds,
             NamedFieldPuns, ApplicativeDo, MultiParamTypeClasses #-}

module Main where

import System.IO
import Control.Applicative
import Servant
import Servant.Types.SourceT
import Network.Wai.Handler.Warp
import qualified Network.Socket
import qualified Data.Streaming.Network
import qualified Options.Applicative as Opt
import System.Process
import Control.Exception hiding (Handler)
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Builder as ByteString.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import System.Exit (die)

data Config =
  Config {
    configBindTarget :: BindTarget,
    configHackagePath :: FilePath,
    configFrontEndPath :: FilePath
  }

type HackageSearchAPI =
  "hackage-search" :>
    (
        "rg" :> Capture "pattern" String :> StreamGet NewlineFraming PlainText RgLineHandle
      :<|>
        Raw -- front-end
    )

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
hackageSearchServer Config{configHackagePath, configFrontEndPath} =
  searchH :<|> frontendH
  where
    searchH :: String -> Handler RgLineHandle
    searchH q = liftIO $ do
      (_, Just hOut, Just hErr, p) <-
        createProcess ((proc "rg" ["--json", "--no-ignore", "--context", "2", "--regexp", q])
          { cwd = Just configHackagePath,
            std_out = CreatePipe,
            std_err = CreatePipe,
            std_in = NoStream
          })
      return (RgLineHandle p hOut hErr)
    frontendH =
      serveDirectoryFileServer configFrontEndPath

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