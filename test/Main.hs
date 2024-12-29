{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import qualified ParserSrtTest

main :: IO ()
main = hspec $ do
  ParserSrtTest.spec

