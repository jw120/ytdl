module Download
  ( download
  ) where

import Data.Maybe (fromMaybe)
import System.Process (callProcess)

import Channels
import qualified ProgramOptions

youtube_dl :: FilePath
youtube_dl = "youtube_dl"

baseUrl :: String
baseUrl = "https://youtube.com"

defaultFormat :: String
defaultFormat = "18"

defaultOutput :: String
defaultOutput = "%(uploader)s-%(upload_date)s-%(title)s.mp4"

defaultDownloadArchive :: String
defaultDownloadArchive = "download-archive"

defaultMaxVideos :: Int
defaultMaxVideos = 25

buildArgs :: ProgramOptions.Config -> Channel -> ([String], Bool)
buildArgs config channel = (args, ProgramOptions.simulate config)
  where
    args =
      [ "--format"
      , fromMaybe defaultFormat (format channel)
      , "--output"
      , defaultOutput
      , "--download-archive"
      , defaultDownloadArchive
      , "--playlist-items"
      , "1-" ++ (show defaultMaxVideos)
      , baseUrl ++ (url channel)
      ]

download :: ProgramOptions.Config -> Channel -> IO ()
download config channel = do
  let (args, simulate) = buildArgs config channel
  -- if simulate
  putStrLn . unwords $ (youtube_dl :  args)
  -- else
  --  callProcess youtube_dl args
