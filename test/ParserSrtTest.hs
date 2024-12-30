{-# LANGUAGE OverloadedStrings #-}

module ParserSrtTest (main, spec) where

import qualified Data.Text as T
import Test.Hspec
import YT (parseSrtToText)

-- Test SRT content from the beginning of the file
testSrtContent :: [T.Text]
testSrtContent =
  [ "1"
  , "00:00:05,000 --> 00:00:09,070"
  , "hi welcome to another video so deep seek "
  , ""
  , "2"
  , "00:00:09,070 --> 00:00:09,080"
  , "hi welcome to another video so deep seek"
  , ""
  , "3"
  , "00:00:09,080 --> 00:00:12,390"
  , "hi welcome to another video so deep seek"
  , "V3 has been recently launched and it's a "
  , "4"
  , "00:00:12,390 --> 00:00:12,400"
  , "V3 has been recently launched and it's a"
  , ""
  , "5"
  , "00:00:12,400 --> 00:00:14,669"
  , "V3 has been recently launched and it's a"
  , "really good model especially because"
  , "6"
  , "00:00:14,669 --> 00:00:14,679"
  , "really good model especially because"
  , ""
  , "7"
  , "00:00:14,679 --> 00:00:17,910"
  , "really good model especially because"
  , "it's fully open weights and their API is"
  , "8"
  , "00:00:17,910 --> 00:00:17,920"
  , "it's fully open weights and their API is"
  , ""
  , "9"
  , "00:00:17,920 --> 00:00:21,550"
  , "it's fully open weights and their API is"
  , "dirt cheap which is pretty great to see"
  , "10"
  , "00:00:21,550 --> 00:00:21,560"
  , "dirt cheap which is pretty great to see"
  , ""
  , "11"
  , "00:00:21,560 --> 00:00:23,990"
  , "dirt cheap which is pretty great to see"
  , "not just that you can currently use the"
  , "12"
  , "00:00:23,990 --> 00:00:24,000"
  , "not just that you can currently use the"
  , ""
  , "13"
  , "00:00:24,000 --> 00:00:27,429"
  , "not just that you can currently use the"
  , "Deep seek V3 model for free via"
  , "14"
  , "00:00:27,429 --> 00:00:27,439"
  , "Deep seek V3 model for free via"
  , ""
  , "15"
  , "00:00:27,439 --> 00:00:30,230"
  , "Deep seek V3 model for free via"
  , "hyperbolic yes although you won't see"
  , "16"
  , "00:00:30,230 --> 00:00:30,240"
  , "hyperbolic yes although you won't see"
  , ""
  , "17"
  , "00:00:30,240 --> 00:00:32,310"
  , "hyperbolic yes although you won't see"
  , "deep seek in hyperbolic model section"
  , "18"
  , "00:00:32,310 --> 00:00:32,320"
  , "deep seek in hyperbolic model section"
  ]

-- Expected output after deduplication
expectedOutput :: [T.Text]
expectedOutput =
  [ "hi welcome to another video so deep seek"
  , ""
  , "V3 has been recently launched and it's a"
  , "really good model especially because"
  , "it's fully open weights and their API is"
  , "dirt cheap which is pretty great to see"
  , "not just that you can currently use the"
  , "Deep seek V3 model for free via"
  , "hyperbolic yes although you won't see"
  , "deep seek in hyperbolic model section"
  ]

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseSrtToText" $ do
    it "removes duplicate lines while preserving structure" $ do
      let result = parseSrtToText testSrtContent
      result `shouldBe` expectedOutput

    it "maintains order of unique lines" $ do
      let result = parseSrtToText testSrtContent
      -- Check the order of non-empty lines
      filter (not . T.null) result `shouldBe` 
        [ "hi welcome to another video so deep seek"
        , "V3 has been recently launched and it's a"
        , "really good model especially because"
        , "it's fully open weights and their API is"
        , "dirt cheap which is pretty great to see"
        , "not just that you can currently use the"
        , "Deep seek V3 model for free via"
        , "hyperbolic yes although you won't see"
        , "deep seek in hyperbolic model section"
        ]
