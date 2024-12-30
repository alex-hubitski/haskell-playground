{-# LANGUAGE OverloadedStrings #-}

module ParserSrtTest (main, spec) where

import qualified Data.Text as T
import Test.Hspec
import YT (parseSrtToText)

-- Test SRT content from the beginning of the file
testSrtContent :: [T.Text]
testSrtContent =
  [ "1"
  , "00:00:00,000 --> 00:00:03,540"
  , ""
  , "[Music]"
  , ""
  , "2"
  , "00:00:03,540 --> 00:00:03,550"
  , "[Music]"
  , ""
  , "3" 
  , "00:00:03,550 --> 00:00:04,990"
  , "[Music]"
  , "[Applause]"
  , ""
  , "4"
  , "00:00:04,990 --> 00:00:05,000"
  , "[Applause]"
  , ""
  , "5"
  , "00:00:05,000 --> 00:00:07,230"
  , "[Applause]"
  , "hi welcome to another"
  , ""
  , "6"
  , "00:00:07,230 --> 00:00:07,240"
  , "hi welcome to another"
  , ""
  , "7"
  , "00:00:07,240 --> 00:00:11,270"
  , "hi welcome to another"
  , "video so deep seek V3 has been making a"
  , ""
  , "8"
  , "00:00:11,270 --> 00:00:11,280"
  , "video so deep seek V3 has been making a"
  , ""
  , "9"
  , "00:00:11,280 --> 00:00:13,950"
  , "video so deep seek V3 has been making a"
  , "lot of hype these days and I really like"
  , ""
  , "10"
  , "00:00:13,950 --> 00:00:13,960"
  , "lot of hype these days and I really like"
  , ""
  , "11"
  , "00:00:13,960 --> 00:00:16,510"
  , "lot of hype these days and I really like"
  , "it because it truly deserves it and it's"
  , ""
  , "12"
  , "00:00:16,510 --> 00:00:16,520"
  , "it because it truly deserves it and it's"
  , ""
  , "13"
  , "00:00:16,520 --> 00:00:20,590"
  , "it because it truly deserves it and it's"
  , "just a very good model I mean the API is"
  , ""
  , "14"
  , "00:00:20,590 --> 00:00:20,600"
  , "just a very good model I mean the API is"
  ]

-- Expected output after deduplication and bracket removal
expectedOutput :: [T.Text]
expectedOutput =
  [ "hi welcome to another"
  , "video so deep seek V3 has been making a"
  , "lot of hype these days and I really like"
  , "it because it truly deserves it and it's"
  , "just a very good model I mean the API is"
  ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseSrtToText" $ do
    it "removes duplicate lines while preserving structure" $ do
      let result = parseSrtToText testSrtContent
      result `shouldBe` expectedOutput

    it "maintains order of unique lines and removes bracketed content" $ do
      let result = parseSrtToText testSrtContent
      -- Check the order of non-empty lines
      filter (not . T.null) result `shouldBe` expectedOutput

    it "removes bracketed content" $ do
      let result = parseSrtToText ["[Music]", "[Applause]", "hello [world] test"]
      result `shouldBe` ["hello test"]
