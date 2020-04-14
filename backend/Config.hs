module Config where

data Config =
  Config {
    configPort :: Int,
    configFrontEndPath :: FilePath
  }
