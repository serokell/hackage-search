{-# LANGUAGE TypeFamilies, TypeOperators, PolyKinds, DataKinds,
             NamedFieldPuns, ApplicativeDo, MultiParamTypeClasses #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Data.ByteString.Char8 as ByteString.Char8
import System.Exit (die)

newtype PackageName = PackageName Text

instance JSON.FromJSON PackageName where
  parseJSON (JSON.Object v) = do
    name <- v JSON..: Text.pack "packageName"
    return (PackageName name)
  parseJSON invalid =
    JSON.prependFailure "parsing PackageName failed, " $
    JSON.typeMismatch "Object" invalid

main :: IO ()
main = do
  http_manager <- HTTP.newTlsManager
  package_names <- requestPackages http_manager
  print (map (\(PackageName name) -> name) package_names)

requestPackages :: HTTP.Manager -> IO [PackageName]
requestPackages http_manager = do
  init_req <- HTTP.parseRequest "https://hackage.haskell.org/packages/"
  let req = init_req { HTTP.requestHeaders = [header_accept_json] }
  response <- HTTP.httpLbs req http_manager
  case JSON.decode (HTTP.responseBody response) of
    Nothing -> die "requestPackages: could not parse the response"
    Just r -> return r

header_accept_json :: HTTP.Header
header_accept_json = (HTTP.hAccept, ByteString.Char8.pack "application/json")
