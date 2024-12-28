module Main where

import qualified Brick
import Graphics.Vty.Platform.Unix (mkVty)
import Graphics.Vty (defaultConfig)
import CommandLineParsing (Options (..), parseOptions)
import qualified YT
import Types (AppState(..))
import UI
import qualified Data.Vector as Vec

main :: IO ()
main = do
  options <- parseOptions
  let videoUrl = url options
      startIndex = playlistStart options
      videoCount = count options

  putStrLn $ "Fetching videos from: " ++ videoUrl
  result <- YT.fetchVideoMetadata videoUrl startIndex videoCount
  case result of
    Left e -> putStrLn $ "Error: " ++ show e
    Right videoList -> do
      let appState = AppState
            { videos = Vec.fromList videoList
            , selected = 0
            , showingDetails = False
            , windowHeight = 0
            , subtitles = Nothing
            }
      
      -- Initialize vty
      let buildVty = mkVty defaultConfig
      initialVty <- buildVty
      
      -- Run the brick application with customMain
      _ <- Brick.customMain initialVty buildVty Nothing app appState
      putStrLn "Done!"
