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
import qualified Data.Set as Set
import Data.Text (Text)
import Text.Regex.TDFA ((=~))

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
          return $ formatOutput paragraphs
        ExitFailure catCode ->
          error $ "cat failed with code " ++ show catCode ++ ": " ++ catErrors
    ExitFailure code ->
      error $ "yt-dlp failed with code " ++ show code ++ ": " ++ errors

-- Helper function to check if a line is a timestamp
isTimestamp :: T.Text -> Bool
isTimestamp line = "-->" `T.isInfixOf` line

-- A parser that reads SRT lines in blocks and converts them to plain text.
parseSrtToText :: [T.Text] -> [T.Text]
parseSrtToText lines = go (map removeBracketedText lines) Set.empty [] False
  where
    go [] _ acc _ = reverse $ filter (not . isTimestamp) acc
    go (x:xs) seen acc lastWasEmpty
      | isBlockNumber x || isTimestamp x =
          -- Skip the index line and timestamp line
          go xs seen acc False
      | T.null x =
          -- If we encounter an empty line, skip it
          go xs seen acc lastWasEmpty
      | T.null (T.strip x) =
          -- Skip empty or whitespace-only lines
          go xs seen acc lastWasEmpty
      | otherwise =
          -- Collect the subtitle text until the next block number or timestamp
          let (block, rest) = break (\line -> isBlockNumber line || isTimestamp line) (x:xs)
              -- Filter duplicates while preserving order
              (uniqueBlock, newSeen) = foldr 
                (\line (lines, seenSet) ->
                  if line `Set.member` seenSet || T.null (T.strip line)
                  then (lines, seenSet)
                  else (line : lines, Set.insert line seenSet)
                )
                ([], seen)
                block
          in go rest newSeen (uniqueBlock ++ acc) False

-- Helper function to remove square brackets and their contents from text
removeBracketedText :: T.Text -> T.Text
removeBracketedText line = 
    let result = go (T.unpack line) False ""
    in T.strip $ T.pack result
  where
    go [] _ acc = reverse acc
    go (c:cs) True acc
      | c == ']'  = go cs False acc  -- end of bracketed section
      | otherwise = go cs True acc    -- skip chars inside brackets
    go (c:cs) False acc
      | c == '['  = go cs True acc    -- start of bracketed section
      | otherwise = go cs False (c:acc)

formatSubtitles :: Int -> T.Text -> [T.Text]
formatSubtitles width text = wrapText width text

wrapText :: Int -> T.Text -> [T.Text]
wrapText width text = go (T.words text) []
  where
    go [] acc = [T.unwords acc]  -- Return the last line
    go words acc =
      let (line, rest) = break (\w -> T.length (T.unwords (acc ++ [w])) > width) words
      in T.unwords acc : go rest (acc ++ line)

-- New function to ensure consistent line endings and format output
formatOutput :: [T.Text] -> T.Text
formatOutput = T.unlines . map (T.justifyLeft 80 ' ')

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
