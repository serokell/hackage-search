module Main where

import Control.Applicative
import Data.Text
import Servant
import Network.Wai.Handler.Warp
import Data.Aeson (ToJSON(toJSON))
import qualified Data.Aeson as JSON
import qualified Data.Vector as Vector
import qualified Options.Applicative as Opt

import Config

data SearchResults = SearchResults

instance ToJSON SearchResults where
  toJSON SearchResults =
    JSON.Array (Vector.fromList [])

type HackageSearchAPI =
    "search" :> Capture "query" Text :> Get '[JSON] SearchResults
  :<|>
    Raw -- front-end

hackageSearchAPI :: Proxy HackageSearchAPI
hackageSearchAPI = Proxy

hackageSearchServer :: Config -> Server HackageSearchAPI
hackageSearchServer Config{configFrontEndPath} =
  searchH :<|> frontendH
  where
    searchH q = do
      return SearchResults
    frontendH =
      serveDirectoryFileServer configFrontEndPath

configOptP :: Opt.Parser Config
configOptP = do
  configPort <- Opt.option Opt.auto (Opt.long "port" <> Opt.metavar "NNNN")
  configFrontEndPath <- Opt.strOption (Opt.long "frontend" <> Opt.metavar "PATH")
  pure Config{configPort, configFrontEndPath}

main :: IO ()
main = do
  config@Config{configPort} <-
    Opt.execParser $
      Opt.info (configOptP <**> Opt.helper)
        (Opt.fullDesc <> Opt.header "Hackage Search")
  run configPort (serve hackageSearchAPI (hackageSearchServer config))
