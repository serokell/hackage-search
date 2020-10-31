{-# LANGUAGE TypeFamilies, TypeOperators, PolyKinds, DataKinds,
             NamedFieldPuns, ApplicativeDo, MultiParamTypeClasses,
             DerivingVia, TypeApplications #-}

module Main (main) where

import System.IO
import System.IO.Error
import Control.Applicative
import Control.Monad
import Control.Concurrent (threadDelay)
import Servant
import Servant.Types.SourceT
import Servant.HTML.Blaze (HTML)
import Network.Wai.Handler.Warp
import Data.Monoid
import qualified Network.Socket
import qualified Data.Streaming.Network
import qualified Options.Applicative as Opt
import System.Process
import Control.DeepSeq (rnf)
import Control.Exception hiding (Handler)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encoding.Internal as JSON (pairStr, list)
import qualified Data.ByteString.Builder as ByteString.Builder
import qualified Data.ByteString.Lazy as ByteString.Lazy
import System.FilePath ((</>))
import qualified System.FilePath as FilePath
import qualified Distribution.Package as Cabal
import qualified Distribution.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Data.Map as Map
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H.A

data Config =
  Config {
    configBindTarget :: BindTarget,
    configHackagePath :: FilePath,
    configFrontEndPath :: FilePath
  }

type HackageSearchAPI =
    "rg" :> Capture "pattern" String :> StreamGet NewlineFraming PlainText RgLineHandle
  :<|>
    "viewfile" :> CaptureAll "segments" String :> Get '[HTML] H.Html
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
  toSourceIO (RgLineHandle p hOut hErr) = fromStepT (go 0 processRg)
    where
      go i (ProcessRgYield s next) = Yield s (go i next)
      go i (ProcessRgPause next) = Effect $ do
        threadDelay (slowdown i * 1000) -- See Note [Rate limiting]
        return $ go (i+1) next
      go i (ProcessRgRequest f) = Effect $ do
        eof <- hIsEOF hOut
        if eof then do
          err <- Text.hGetContents hErr
          if Text.null err
            then return stop
            else return (Yield (jsonEncodeErr err) stop)
        else do
          s <- Text.hGetLine hOut
          return $
            case JSON.eitherDecode' (ByteString.Lazy.fromStrict (Text.encodeUtf8 s)) of
              Left err -> go i (ProcessRgFail (Text.pack err))
              Right rg_out -> go i (f rg_out)
      go _ (ProcessRgFail err) = Yield (jsonEncodeErr err) stop
      go _ (ProcessRgComplete matches) = Yield (jsonEncodeSummary matches) stop
      stop = Effect $ do
        terminateProcess p
        hClose hOut
        hClose hErr
        return Stop

-- Input: iteration number
-- Output: delay in milliseconds
slowdown :: Int -> Int
slowdown i = min 1000 (i `quot` 100)

{- Note [Rate limiting]
~~~~~~~~~~~~~~~~~~~~~~~
If the user query matches many files, then processing the request and producing
the results will be very CPU-intensive. And that's bad for several reasons:

  1. There's no point producing results faster than the front-end can handle them
  2. We don't want to expose a denial-of-service attack surface
  3. The user can't possibly read the results that fast anyway

We mitigate the issue by waiting for longer and longer periods of time for
every next result.
-}

hackageSearchServer :: Config -> Server HackageSearchAPI
hackageSearchServer config =
  searchH :<|> viewfileH :<|> frontendH
  where
    searchH q = liftIO (rgSearch config q)
    viewfileH q = do
      mResult <- liftIO (viewFile config q)
      case mResult of
        Nothing -> throwError err400
        Just r -> return r
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
  let rg_opts = ["--json", "--no-ignore", "--context", "2", "--sort", "path", "--regexp", rg_pattern]
  (_, Just hOut, Just hErr, p) <-
    createProcess ((proc "rg" (rg_opts ++ package_ids))
      { cwd = Just (packagesPath config),
        std_out = CreatePipe,
        std_err = CreatePipe,
        std_in = NoStream
      })
  return (RgLineHandle p hOut hErr)

jsonEncodeErr :: Text -> Text
jsonEncodeErr err =
  jsonEncodingToText $
  JSON.pairs $
    JSON.pairStr "type" (JSON.toEncoding "error") <>
    JSON.pairStr "message" (JSON.toEncoding err)

jsonEncodeSummary :: Int -> Text
jsonEncodeSummary matches =
  jsonEncodingToText $
  JSON.pairs $
    JSON.pairStr "type" (JSON.toEncoding "summary") <>
    JSON.pairStr "matches" (JSON.toEncoding matches)

jsonEncodingToText :: JSON.Encoding -> Text
jsonEncodingToText =
      Text.decodeUtf8 .
      ByteString.Lazy.toStrict .
      ByteString.Builder.toLazyByteString .
      JSON.fromEncoding

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

---------------------------- Reading rg output --------------------------------

data RgOut
  = RgOutBegin {
      rg_out_begin_path :: !RgOutData
    }
  | RgOutMatch {  -- also represents a RgOutContext
      rg_out_match_path :: !RgOutData,
      rg_out_match_lines :: !RgOutData,
      rg_out_match_line_number :: !Int,
      rg_out_match_absolute_offset :: !Int,
      rg_out_match_submatches :: ![RgOutSubmatch]
    }
  | RgOutEnd {
      rg_out_end_path :: !RgOutData,
      rg_out_end_stats :: !RgOutStats
    }
  | RgOutSummary {
      rg_out_summary_elapsed_total :: !RgOutDuration,
      rg_out_summary_stats :: !RgOutStats
    }

instance JSON.FromJSON RgOut where
  parseJSON =
    JSON.withObject "RgOut" $ \j -> do
      t <- j JSON..: Text.pack "type"
      d <- j JSON..: Text.pack "data"
      parseDataJSON t d
    where
      parseDataJSON "begin"   = parseBeginJSON
      parseDataJSON "match"   = parseMatchOrContextJSON
      parseDataJSON "context" = parseMatchOrContextJSON
      parseDataJSON "end"     = parseEndJSON
      parseDataJSON "summary" = parseSummaryJSON
      parseDataJSON _ = \_ -> fail "Unknown type"

      parseBeginJSON =
        JSON.withObject "RgOutBegin" $ \j -> do
          rg_out_begin_path <- j JSON..: Text.pack "path"
          pure RgOutBegin{ rg_out_begin_path }

      parseMatchOrContextJSON =
        JSON.withObject "RgOutMatch" $ \j -> do
          rg_out_match_path <- j JSON..: Text.pack "path"
          rg_out_match_lines <- j JSON..: Text.pack "lines"
          rg_out_match_line_number <- j JSON..: Text.pack "line_number"
          rg_out_match_absolute_offset <- j JSON..: Text.pack "absolute_offset"
          rg_out_match_submatches <- j JSON..: Text.pack "submatches"
          pure RgOutMatch{
            rg_out_match_path,
            rg_out_match_lines,
            rg_out_match_line_number,
            rg_out_match_absolute_offset,
            rg_out_match_submatches
          }

      parseEndJSON =
        JSON.withObject "RgOutEnd" $ \j -> do
          rg_out_end_path <- j JSON..: Text.pack "path"
          rg_out_end_stats <- j JSON..: Text.pack "stats"
          pure RgOutEnd{
              rg_out_end_path,
              rg_out_end_stats
            }

      parseSummaryJSON =
        JSON.withObject "RgOutSummary" $ \j -> do
          rg_out_summary_elapsed_total <- j JSON..: Text.pack "elapsed_total"
          rg_out_summary_stats <- j JSON..: Text.pack "stats"
          pure RgOutSummary{
              rg_out_summary_elapsed_total,
              rg_out_summary_stats
            }

data RgOutSubmatch =
  RgOutSubmatch {
    -- rg_out_submatch_match :: !RgOutData,
    rg_out_submatch_start :: !Int,
    rg_out_submatch_end :: !Int
  }

instance JSON.FromJSON RgOutSubmatch where
  parseJSON =
    JSON.withObject "RgOutSubmatch" $ \j -> do
      -- rg_out_submatch_match <- j JSON..: Text.pack "match"
      rg_out_submatch_start <- j JSON..: Text.pack "start"
      rg_out_submatch_end <- j JSON..: Text.pack "end"
      pure RgOutSubmatch{
          -- rg_out_submatch_match,
          rg_out_submatch_start,
          rg_out_submatch_end
        }

data RgOutStats =
  RgOutStats {
    rg_out_stats_elapsed :: !RgOutDuration,
    rg_out_stats_searches :: !Int,
    rg_out_stats_searches_with_match :: !Int,
    rg_out_stats_bytes_searched :: !Int,
    rg_out_stats_bytes_printed :: !Int,
    rg_out_stats_matched_lines :: !Int,
    rg_out_stats_matches :: !Int
  }

instance JSON.FromJSON RgOutStats where
  parseJSON =
    JSON.withObject "RgOutStats" $ \j -> do
      rg_out_stats_elapsed <- j JSON..: Text.pack "elapsed"
      rg_out_stats_searches <- j JSON..: Text.pack "searches"
      rg_out_stats_searches_with_match <- j JSON..: Text.pack "searches_with_match"
      rg_out_stats_bytes_searched <- j JSON..: Text.pack "bytes_searched"
      rg_out_stats_bytes_printed <- j JSON..: Text.pack "bytes_printed"
      rg_out_stats_matched_lines <- j JSON..: Text.pack "matched_lines"
      rg_out_stats_matches <- j JSON..: Text.pack "matches"
      pure RgOutStats{
          rg_out_stats_elapsed,
          rg_out_stats_searches,
          rg_out_stats_searches_with_match,
          rg_out_stats_bytes_searched,
          rg_out_stats_bytes_printed,
          rg_out_stats_matched_lines,
          rg_out_stats_matches
        }

data RgOutDuration =
  RgOutDuration {
    rg_out_duration_human :: !Text,
    rg_out_duration_nanos :: !Int,
    rg_out_duration_secs :: !Int
  }

instance JSON.FromJSON RgOutDuration where
  parseJSON =
    JSON.withObject "RgOutDuration" $ \j -> do
      rg_out_duration_human <- j JSON..: Text.pack "human"
      rg_out_duration_nanos <- j JSON..: Text.pack "nanos"
      rg_out_duration_secs <- j JSON..: Text.pack "secs"
      pure RgOutDuration{
          rg_out_duration_human,
          rg_out_duration_nanos,
          rg_out_duration_secs
        }

data RgOutData
  = RgOutDataText !Text
  | RgOutDataBytes !Text

instance JSON.FromJSON RgOutData where
  parseJSON =
    JSON.withObject "RgOutData" $ \j -> do
      m_rg_text <- j JSON..:? Text.pack "text"
      case m_rg_text of
        Just rg_text -> return (RgOutDataText rg_text)
        Nothing -> do
          rg_bytes <- j JSON..: Text.pack "bytes"
          return (RgOutDataBytes rg_bytes)

--------------------------- Processing rg output -------------------------------

data FileId = FileId !Cabal.PackageId !Text
  deriving (Eq, Ord)

pprFileId :: FileId -> String
pprFileId (FileId pkg_id file_path) = Cabal.prettyShow pkg_id ++ ":" ++ Text.unpack file_path

newtype PkgResultMap = PkgResultMap (Map.Map FileId PkgResultFile)

instance Semigroup PkgResultMap where
  PkgResultMap m1 <> PkgResultMap m2 = PkgResultMap (Map.unionWith (<>) m1 m2)

instance Monoid PkgResultMap where
  mempty = PkgResultMap Map.empty

encodePkgResult :: FileId -> PkgResultFile -> JSON.Encoding
encodePkgResult (FileId pkg_id file_path) pkg_result_file =
  JSON.pairs $
    JSON.pairStr "type" (JSON.toEncoding "file") <>
    JSON.pairStr "package" (JSON.toEncoding (Cabal.prettyShow pkg_id)) <>
    JSON.pairStr "path" (JSON.toEncoding (Text.unpack file_path)) <>
    JSON.pairStr "lines" (encodePkgResultFile pkg_result_file)

-- DList
newtype PkgResultFile = PkgResultFile ([PkgResultLine] -> [PkgResultLine])
  deriving (Semigroup, Monoid) via Endo [PkgResultLine]

encodePkgResultFile :: PkgResultFile -> JSON.Encoding
encodePkgResultFile (PkgResultFile f) =
  JSON.list encodePkgResultLine (f [])

data PkgResultLine =
  PkgResultLine {
    pkg_result_line_number :: !Int,
    pkg_result_line_parts :: ![PkgResultLinePart]
  }

encodePkgResultLine :: PkgResultLine -> JSON.Encoding
encodePkgResultLine
    PkgResultLine{pkg_result_line_number, pkg_result_line_parts}
  =
    JSON.pairs $
      JSON.pairStr "number" (JSON.toEncoding pkg_result_line_number) <>
      JSON.pairStr "parts" (JSON.list encodePkgResultLinePart pkg_result_line_parts)

data PkgResultLinePart
  = PkgResultLinePartContext !Text  -- surrounding text
  | PkgResultLinePartMatch !Text    -- highlighted part of the match result

encodePkgResultLinePart :: PkgResultLinePart -> JSON.Encoding
encodePkgResultLinePart p = JSON.pairs (JSON.pairStr part_t (JSON.toEncoding part_s))
  where
    (part_t, part_s) = case p of
      PkgResultLinePartContext s -> ("context", s)
      PkgResultLinePartMatch s -> ("match", s)

splitPkgId :: Text -> Either Text FileId
splitPkgId s =
  let (pkg_id_str, slash_path) = Text.break (== '/') s
  in case Cabal.simpleParsec (Text.unpack pkg_id_str) of
    Nothing -> Left (Text.pack ("bad package id in the file path: " ++ show s))
    Just pkg_id -> Right (FileId pkg_id (Text.drop 1 slash_path))

data ProcessRg
  = ProcessRgYield !Text {- lazy tail -} ProcessRg
  | ProcessRgPause {- lazy tail -} ProcessRg
  | ProcessRgRequest !(RgOut -> ProcessRg)
  | ProcessRgFail !Text
  | ProcessRgComplete !Int

processRg :: ProcessRg
processRg = processRgOut' mempty

processRgOut' :: PkgResultMap -> ProcessRg
processRgOut' pkg_result_map = ProcessRgRequest (processRgOut pkg_result_map)

withSplitPkgId :: Text -> (FileId -> ProcessRg) -> ProcessRg
withSplitPkgId rg_path cont =
  case splitPkgId rg_path of
    Left err -> ProcessRgFail err
    Right file_id -> cont file_id

withTextPath :: RgOutData -> (Text -> ProcessRg) -> ProcessRg
withTextPath (RgOutDataBytes _) _ = ProcessRgFail (Text.pack "Invalid file path")
withTextPath (RgOutDataText path) cont = cont path

processRgOut :: PkgResultMap -> RgOut -> ProcessRg
processRgOut pkg_result_map rg_out

  | RgOutBegin{rg_out_begin_path} <- rg_out
  = withTextPath rg_out_begin_path $ \path ->
    withSplitPkgId path $ \file_id -> do
    let pkg_result_map' = initPkgResultFile file_id pkg_result_map
    processRgOut' pkg_result_map'

  | RgOutEnd{rg_out_end_path} <- rg_out
  = withTextPath rg_out_end_path $ \path ->
    withSplitPkgId path $ \file_id ->
    takePkgResultFile file_id pkg_result_map $ \pkg_res pkg_result_map' -> do
    let pkg_res_json = jsonEncodingToText (encodePkgResult file_id pkg_res)
    ProcessRgYield pkg_res_json (processRgOut' pkg_result_map')

  | RgOutMatch{
      rg_out_match_path,
      rg_out_match_lines,
      rg_out_match_line_number,
      rg_out_match_submatches
    } <- rg_out
  = withTextPath rg_out_match_path $ \path ->
    withSplitPkgId path $ \file_id ->
    case rg_out_match_lines of
      RgOutDataBytes _ ->
        -- skip non-UTF8 matches
        ProcessRgPause (processRgOut' pkg_result_map)
      RgOutDataText match_lines -> do
        let pkg_result_line =
              mkPkgResultLine
                rg_out_match_line_number
                match_lines
                rg_out_match_submatches
        let pkg_result_map' =
              addLineToPkgResultFile
                file_id
                pkg_result_line
                pkg_result_map
        let pause | null rg_out_match_submatches = id
                  | otherwise = ProcessRgPause
        pause (processRgOut' pkg_result_map')

  | RgOutSummary{rg_out_summary_stats} <- rg_out
  = ProcessRgComplete (rg_out_stats_matches rg_out_summary_stats)

mkPkgResultLine
  :: Int
  -> Text
  -> [RgOutSubmatch]
  -> PkgResultLine
mkPkgResultLine line_number line all_submatches =
    PkgResultLine {
      pkg_result_line_number = line_number,
      pkg_result_line_parts = filterOut isEmptyPkgResultLinePart (go 0 all_submatches)
    }
  where
    raw_str = Text.encodeUtf8 line
    slice n m = Text.decodeUtf8 (sliceByteString n m raw_str)

    go :: Int -> [RgOutSubmatch] -> [PkgResultLinePart]
    go last_pos (submatch:submatches)
      | RgOutSubmatch{rg_out_submatch_start, rg_out_submatch_end} <- submatch
      = PkgResultLinePartContext (slice last_pos rg_out_submatch_start) :
        PkgResultLinePartMatch (slice rg_out_submatch_start rg_out_submatch_end) :
        go rg_out_submatch_end submatches
    go last_pos [] =
      [PkgResultLinePartContext (Text.decodeUtf8 (ByteString.drop last_pos raw_str))]

sliceByteString :: Int -> Int -> ByteString -> ByteString
sliceByteString n m s = ByteString.take (m-n) (ByteString.drop n s)

filterOut :: (a -> Bool) -> [a] -> [a]
filterOut p = filter (not . p)

isEmptyPkgResultLinePart :: PkgResultLinePart -> Bool
isEmptyPkgResultLinePart (PkgResultLinePartContext t) = Text.null t
isEmptyPkgResultLinePart (PkgResultLinePartMatch t) = Text.null t

initPkgResultFile
  :: FileId
  -> PkgResultMap
  -> PkgResultMap
initPkgResultFile file_id (PkgResultMap m) =
  PkgResultMap (Map.insert file_id (mempty @PkgResultFile) m)

addLineToPkgResultFile
  :: FileId
  -> PkgResultLine
  -> PkgResultMap
  -> PkgResultMap
addLineToPkgResultFile file_id pkg_result_line (PkgResultMap m) =
  PkgResultMap (Map.adjust (<> PkgResultFile (pkg_result_line:)) file_id m)

takePkgResultFile
  :: FileId
  -> PkgResultMap
  -> (PkgResultFile -> PkgResultMap -> ProcessRg)
  -> ProcessRg
takePkgResultFile file_id (PkgResultMap m) cont =
  case Map.lookup file_id m of
    Nothing -> ProcessRgFail (Text.pack ("file not in the result map: " ++ pprFileId file_id))
    Just pkg_result_file -> cont pkg_result_file (PkgResultMap (Map.delete file_id m))

viewFile :: Config -> [String] -> IO (Maybe H.Html)
viewFile config userstring = runMaybeT $ do
  userfilepath <- MaybeT (return (validateFilePath userstring))
  let filepath = packagesPath config </> userfilepath
  contents <- MaybeT (readFileIfExists filepath)
  let html = renderFile userfilepath contents
  MaybeT (return (Just html))

readFileIfExists :: FilePath -> IO (Maybe String)
readFileIfExists path =
  handleJust
    (\e -> if isDoesNotExistError e then Just () else Nothing)
    (\() -> return Nothing)
    (fmap Just (System.IO.readFile path))

-- (hopefully) prevents access outside packagesPath
validateFilePath :: [String] -> Maybe FilePath
validateFilePath path_pieces = do
    mapM_ (\p -> guard (isValidPathPiece p)) path_pieces
    let path = FilePath.joinPath path_pieces
    guard (FilePath.isValid path && FilePath.isRelative path)
    Just path
  where
    isValidPathPiece p
      | [r] <- FilePath.splitPath p,
        r /= ".",
        r /= ".."
      = True
      | otherwise = False

renderFile :: FilePath -> String -> H.Html
renderFile userfilepath s = H.docTypeHtml $ do
  H.head $ do
    H.title (H.toHtml ("Hackage Search: " ++ userfilepath))
    H.style (H.preEscapedToHtml cssStyle)
  H.body $ do
    H.pre $ H.table $ do
      forM_ (zip lineNumbers (lines s)) $ \(lineNumber, line) -> do
        let lineNumberId = H.preEscapedStringValue ("line-" ++ lineNumber)
        H.tr H.! H.A.id lineNumberId $ do
          H.td (H.toHtml lineNumber)
          H.td (H.toHtml line)
  where
    cssStyle =
      "body{background:#fdf6e3}\
      \body,pre{margin:0}\
      \table{border-collapse:collapse;border-right:1px solid #efe9d7}\
      \td:first-child{user-select:none;\
                     \background:#eee;\
                     \color:#888;\
                     \text-align:right;\
                     \padding:3px}\
      \td{padding-left:10px}\
      \:target{background:#e3301b;color:white}"

lineNumbers :: [String]
lineNumbers = map show [1 :: Word ..]
