{-# LANGUAGE OverloadedStrings #-}

module ParserSrtTest (main, spec) where

import qualified Data.Text as T
import Test.Hspec
import YT (parseSrtToText)  -- Replace 'YourModule' with the actual module name

-- Fake SRT content for testing
fakeSrtContent :: [T.Text]
fakeSrtContent =
  [ T.pack "1"
  , T.pack "00:00:01,000 --> 00:00:02,000"
  , T.pack "Hello, world!"
  , T.pack ""
  , T.pack "2"
  , T.pack "00:00:03,000 --> 00:00:04,000"
  , T.pack "This is a test."
  , T.pack ""
  , T.pack "3"
  , T.pack "00:00:05,000 --> 00:00:06,000"
  , T.pack "Goodbye!"
  , T.pack ""
  ]

-- Main function to run the tests
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseSrtToText" $ do
    it "parses SRT content into plain text" $ do
      let result = parseSrtToText fakeSrtContent
      result `shouldBe` [T.pack "Hello, world!", T.pack "This is a test.", T.pack "Goodbye!"]