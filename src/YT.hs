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

import Types (Video(..), parseVideo)



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
  (exitCode, _, _) <- readProcessWithExitCode "yt-dlp" args ""

  case exitCode of
    ExitSuccess -> do
      (catExitCode, output, _) <- readProcessWithExitCode "cat" ["temp_subtitle.en.srt"] ""
      _ <- readProcessWithExitCode "rm" ["temp_subtitle.en.srt"] ""
      case catExitCode of
        ExitSuccess -> do
          let srtLines = T.lines (T.pack output)
          -- Parse the SRT lines into plain text paragraphs
          let paragraphs = parseSrtToText srtLines
          return $ formatOutput paragraphs
        _ -> return "n/a"  -- No subtitles found, return empty text
    _ -> return "n/a"  -- yt-dlp failed to find subtitles, return empty text

-- Helper function to check if a line is a timestamp
isTimestamp :: T.Text -> Bool
isTimestamp line = "-->" `T.isInfixOf` line

-- A parser that reads SRT lines in blocks and converts them to plain text.
parseSrtToText :: [T.Text] -> [T.Text]
parseSrtToText srtLines = go (map removeBracketedText srtLines) Set.empty [] False
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
                (\line (accLines, seenSet) ->
                  if line `Set.member` seenSet || T.null (T.strip line)
                  then (accLines, seenSet)
                  else (line : accLines, Set.insert line seenSet)
                )
                ([], seen)
                block
          in go rest newSeen (reverse uniqueBlock ++ acc) False

-- Helper function to remove square brackets and their contents from text
removeBracketedText :: T.Text -> T.Text
removeBracketedText line = 
    let result = go (T.unpack line) False []
        -- Clean up multiple spaces that might appear after removing brackets
        cleanedResult = unwords . words $ result
    in T.strip $ T.pack cleanedResult
  where
    go [] _ acc = reverse acc
    go (c:cs) True acc
      | c == ']'  = go cs False acc  -- end of bracketed section
      | otherwise = go cs True acc    -- skip chars inside brackets
    go (c:cs) False acc
      | c == '['  = go cs True acc    -- start of bracketed section
      | otherwise = go cs False (c:acc)


-- New function to ensure consistent line endings and format output
formatOutput :: [T.Text] -> T.Text
formatOutput = T.unlines . map (T.justifyLeft 80 ' ')

-- Helper function to check if a line is a digit

-- Check if a line is purely numeric (index).
isBlockNumber :: T.Text -> Bool
isBlockNumber line = T.all (\c -> c >= '0' && c <= '9') (T.strip line)

