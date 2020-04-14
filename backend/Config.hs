module Config where

data Config =
  Config {
    configPort :: Int,
    configHackagePath :: FilePath,
    configFrontEndPath :: FilePath
  }
