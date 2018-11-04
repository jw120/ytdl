module Download
  ( download
  ) where

import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, isNothing)
import System.Process (callProcess)

import Channels
import ProgramOptions

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

-- Is first string a proper (case-insensitive) substring of the second
isSubstring :: String -> String -> Bool
isSubstring [] _ = False
isSubstring pattern s = isInfixOf (map toLower pattern) (map toLower s)

-- True if the config selects all channels
selectsAll :: Config -> Bool
selectsAll config = isNothing n && isNothing t
    where
      n = matchName config
      t = matchTag config

-- Is there a match between the name in the config and the name of the channel
hasNameMatch :: Config -> Channel -> Bool
hasNameMatch config channel = hasNameMatch' (matchName config) (name channel)
    where
      hasNameMatch' :: Maybe String -> Maybe String -> Bool
      hasNameMatch' (Just pattern) (Just s) = isSubstring pattern s
      hasNameMatch' _ _ = False

-- Is there a match between the tag name in the config and a tag in the channel
hasTagMatch :: Config -> Channel -> Bool
hasTagMatch config channel = hasTagMatch' (matchTag config) (tags channel)
    where
      hasTagMatch' :: Maybe String -> Maybe [String] -> Bool
      hasTagMatch' (Just pattern) (Just xs) = any (isSubstring pattern) xs
      hasTagMatch' _ _ = False

buildArgs :: ProgramOptions.Config -> Channel -> ([String], Bool, Bool)
buildArgs config channel = (args, active, simulate config)
  where
    active = selectsAll config || hasNameMatch config channel ||  hasTagMatch config channel
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
  let (args, active, simulate) = buildArgs config channel
  case (active, simulate) of
    (True, True) -> putStrLn . unwords $ (youtube_dl :  args)
    (True, False) -> putStrLn . unwords $ ("live" : youtube_dl :  args)
    (False, _) -> return ()
