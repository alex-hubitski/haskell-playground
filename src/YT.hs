{-# LANGUAGE OverloadedStrings #-}
module YT
  ( fetchVideoMetadata
  , fetchSubtitles
  , parseSrtToText
  ) where

import Control.Exception (try, SomeException)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (decode, FromJSON(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Process
import Data.Maybe (mapMaybe)
import System.Exit (ExitCode(..))

import Types (Video(..))
import Parser (parseVideo)

instance FromJSON Video where
  parseJSON = parseVideo


fetchVideoMetadata :: String -> Int -> Int -> IO (Either SomeException [Video])
fetchVideoMetadata videoUrl playlistStart count = try $ do
  (exitCode, output, errors) <- readProcessWithExitCode "yt-dlp" args ""

  putStrLn "\n=== Raw yt-dlp output ==="
  --putStrLn output
  putStrLn "=== End raw output ===\n"

  case exitCode of
    ExitSuccess ->
      let jsonLines = BL.split 10 . BL.fromStrict . TE.encodeUtf8 . T.pack $ output
          videos = mapMaybe decode jsonLines
      in return videos
    ExitFailure code ->
      error $ "yt-dlp failed with code " ++ show code ++ ": " ++ errors

  where
    args = [ "--flat-playlist"
          , "--dump-json"
          , "--playlist-start", show playlistStart
          , "--playlist-end", show (playlistStart + count - 1)
          , videoUrl
          ]

fetchSubtitles :: String -> IO (Either SomeException T.Text)
fetchSubtitles videoUrl = try $ do
  let args = [ "--skip-download"
             , "--write-sub"
             , "--write-auto-sub"
             , "--sub-lang", "en"
             , "--sub-format", "best"
             , "--convert-subs", "srt"
             , "-o", "temp_subtitle"
             , videoUrl
             ]
  (exitCode, _, errors) <- readProcessWithExitCode "yt-dlp" args ""

  case exitCode of
    ExitSuccess -> do
      (catExitCode, output, catErrors) <- readProcessWithExitCode "cat" ["temp_subtitle.en.srt"] ""
      _ <- readProcessWithExitCode "rm" ["temp_subtitle.en.srt"] ""
      case catExitCode of
        ExitSuccess -> do
          let srtLines = T.lines (T.pack output)
          -- Parse the SRT lines into plain text paragraphs
          let paragraphs = parseSrtToText srtLines
          let formattedSubtitles = formatSubtitles 80 (T.unlines paragraphs)
          return $ T.unlines formattedSubtitles
        ExitFailure catCode ->
          error $ "cat failed with code " ++ show catCode ++ ": " ++ catErrors
    ExitFailure code ->
      error $ "yt-dlp failed with code " ++ show code ++ ": " ++ errors

-- A parser that reads SRT lines in blocks and converts them to plain text.
-- If a line is a number, we skip it and the next line (assumed to be a timestamp),
-- then read text lines until we hit a blank line.
parseSrtToText :: [T.Text] -> [T.Text]
parseSrtToText [] = []
parseSrtToText (x:xs)
  | T.all isDigit x =
      -- Skip the index line (x) and the timestamp line (next line)
      parseSrtToText (drop 1 xs)  -- Skip the next line (timestamp)
  | T.null x =
      -- If we encounter an empty line, continue parsing
      parseSrtToText xs
  | otherwise =
      -- Collect the subtitle text until the next empty line
      let (block, rest) = break T.null (x:xs)  -- Collect lines until an empty line
      in T.unlines block : parseSrtToText (dropWhile T.null rest)  -- Skip empty lines after the block

-- Helper function to check if a line is a digit
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

-- Check if a line is purely numeric (index).
isBlockNumber :: T.Text -> Bool
isBlockNumber line = T.all (\c -> c >= '0' && c <= '9') (T.strip line)

-- Read lines until a blank line (end of block).
readBlock :: [T.Text] -> ([T.Text], [T.Text])
readBlock [] = ([], [])
readBlock (l:ls)
  | T.null (T.strip l) = ([], ls)  -- blank line => end of block
  | otherwise =
      let (subsequent, rest) = readBlock ls
      in (l : subsequent, rest)

-- Skip n lines, returning the remainder.
skip :: Int -> [T.Text] -> ([T.Text], [T.Text])
skip 0 xs = ([], xs)
skip _ [] = ([], [])
skip n (_:xs) = skip (n - 1) xs

formatSubtitles :: Int -> T.Text -> [T.Text]
formatSubtitles width text = wrapText width (T.unlines (map T.strip (T.lines text)))

wrapText :: Int -> T.Text -> [T.Text]
wrapText width text = go (T.words text) []
  where
    go [] acc = [T.unwords (reverse acc)]  -- Return the last line
    go words acc =
      let (line, rest) = break (\w -> T.length (T.unwords (w : acc)) > width) words
      in T.unwords (reverse acc) : go rest (line ++ acc)
