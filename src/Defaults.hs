-- Default values for parameters

module Defaults
  ( channelsFile
  , configFile
  , maxVideos
  , targetDir
  , videoFormat
  , ytdlCommand
  )
where

channelsFile :: FilePath
channelsFile = "~/.ytdl_channels.json"

configFile :: FilePath
configFile = "~/.ytdl_config.json"

maxVideos :: Int
maxVideos = 10

targetDir :: Maybe String
targetDir = Nothing

videoFormat :: String
videoFormat = "18"

ytdlCommand :: String
ytdlCommand = "yt-dlp"
