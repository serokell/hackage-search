module Main where

import System.IO
import Control.Applicative
import Servant
import Servant.Types.SourceT
import Network.Wai.Handler.Warp
import qualified Data.Aeson as JSON
import qualified Options.Applicative as Opt
import qualified Data.ByteString as ByteString
import System.Process
import Control.Monad.IO.Class

import Config

type HackageSearchAPI =
    "rg" :> Capture "pattern" String :> StreamGet NewlineFraming JSON JsonHandle
  :<|>
    Raw -- front-end

hackageSearchAPI :: Proxy HackageSearchAPI
hackageSearchAPI = Proxy

newtype JsonHandle = JsonHandle Handle

instance ToSourceIO JSON.Value JsonHandle where
  toSourceIO (JsonHandle h) = fromStepT go
    where
      go = Effect $ do
        eof <- hIsEOF h
        if eof then return Stop else do
          s <- ByteString.hGetLine h
          j <-
            case JSON.decodeStrict s of
              Nothing -> error ("JsonHandle: not JSON:\n" ++ show s)
              Just j -> return j
          return $ Yield j go

hackageSearchServer :: Config -> Server HackageSearchAPI
hackageSearchServer Config{configHackagePath, configFrontEndPath} =
  searchH :<|> frontendH
  where
    searchH :: String -> Handler JsonHandle
    searchH q = liftIO $ do
      (_, Just outH, _, _) <-
        createProcess ((proc "rg" ["--json", "--no-ignore", "--regexp", q])
          { cwd = Just configHackagePath,
            std_out = CreatePipe
          })
      return (JsonHandle outH)
    frontendH =
      serveDirectoryFileServer configFrontEndPath

configOptP :: Opt.Parser Config
configOptP = do
  configPort <- Opt.option Opt.auto (Opt.long "port" <> Opt.metavar "NNNN")
  configHackagePath <- Opt.strOption (Opt.long "hackage" <> Opt.metavar "PATH")
  configFrontEndPath <- Opt.strOption (Opt.long "frontend" <> Opt.metavar "PATH")
  pure Config{configPort, configHackagePath, configFrontEndPath}

main :: IO ()
main = do
  config@Config{configPort} <-
    Opt.execParser $
      Opt.info (configOptP <**> Opt.helper)
        (Opt.fullDesc <> Opt.header "Hackage Search")
  run configPort (serve hackageSearchAPI (hackageSearchServer config))
