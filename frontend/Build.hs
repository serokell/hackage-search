{-# LANGUAGE OverloadedStrings, ApplicativeDo, NamedFieldPuns #-}

import Control.Applicative
import System.Process
import System.FilePath
import System.Environment
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Options.Applicative as Opt

data Config = Config { srcDir :: FilePath, outDir :: FilePath }

configOptP :: Opt.Parser Config
configOptP = do
  srcDir <- Opt.strOption (Opt.long "src" <> Opt.metavar "PATH")
  outDir <- Opt.strOption (Opt.long "out" <> Opt.metavar "PATH")
  pure Config{srcDir, outDir}

main :: IO ()
main = do
  Config{srcDir, outDir} <-
    Opt.execParser $
      Opt.info (configOptP <**> Opt.helper)
        (Opt.fullDesc <> Opt.header "Build the Hackage Search Front End")
  callProcess "tsc"
    [ "--alwaysStrict", "-p", srcDir,
      "--outFile", "all.js" ]
  js_out <-
    readProcess "closure-compiler"
      [ "--language_in", "ECMASCRIPT6_STRICT",
        "all.js" ]
      ""
  css_out <-
    readProcess "sass"
      [ "--style", "compressed",
        srcDir </> "main.scss" ]
      ""
  html_src <- Text.readFile (srcDir </> "index.html")
  let html_out =
        insert_css (Text.pack css_out) $
        insert_js (Text.pack js_out) $
        html_src
  Text.writeFile (outDir </> "index.html") html_out

insert_css css = Text.replace "<style></style>" ("<style>" <> css <> "</style>")
insert_js js = Text.replace "<script></script>" ("<script>" <> js <> "</script>")
