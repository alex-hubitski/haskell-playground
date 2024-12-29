{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Test.Hspec
import YourModule (parseSrtToText)  -- Replace 'YourModule' with the actual module name

-- Fake SRT content for testing
fakeSrtContent :: [T.Text]
fakeSrtContent =
  [ "1"
  , "00:00:01,000 --> 00:00:02,000"
  , "Hello, world!"
  , ""
  , "2"
  , "00:00:03,000 --> 00:00:04,000"
  , "This is a test."
  , ""
  , "3"
  , "00:00:05,000 --> 00:00:06,000"
  , "Goodbye!"
  , ""
  ]

-- Main function to run the tests
main :: IO ()
main = hspec $ do
  describe "parseSrtToText" $ do
    it "parses SRT content into plain text" $ do
      let result = parseSrtToText fakeSrtContent
      result `shouldBe` ["Hello, world!", "This is a test.", "Goodbye!"]
