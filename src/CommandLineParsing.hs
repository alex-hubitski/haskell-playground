module CommandLineParsing (Options (..), parseOptions) where

import Options.Applicative

data Options = Options
  { url :: String
  , playlistStart :: Int
  , count :: Int
  } deriving (Show, Eq)

parseOptions :: IO Options
parseOptions = execParser opts

opts :: ParserInfo Options
opts = info (optionParser <**> helper)
  ( fullDesc
  <> progDesc "Fetch video metadata from a YouTube channel or playlist"
  <> header "youtube-playlist-downloader - a simple YouTube metadata fetcher"
  )

optionParser :: Parser Options
optionParser = Options
  <$> argument str (
        metavar "URL" 
        <> help "YouTube channel or playlist URL"
      )
  <*> option auto (
        long "playlist-start"
        <> short 's'
        <> value 1
        <> metavar "START"
        <> help "First video number to fetch (default: 1)"
      )
  <*> option auto (
        long "count"
        <> short 'c'
        <> value 10
        <> metavar "COUNT"
        <> help "Number of videos to fetch (default: 10)"
      ) 