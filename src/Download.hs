-- Code to run each downlowd

module Download
  ( downloadAll
  ) where

import           Control.Monad  (when)
import           Data.Char      (toLower)
import           Data.List      (isInfixOf)
import           Data.Maybe     (fromMaybe, isNothing)
import           System.Exit    (ExitCode(..))
import           System.IO      (hFlush, stdout)
import           System.Process (rawSystem)

import           Channels       (Channel (..))
import           Config         (Config (..))

--
-- Standard values for the download
---

youtube_dl :: FilePath
youtube_dl = "youtube-dl"

baseUrl :: String
baseUrl = "https://youtube.com/"

downloadArchive :: String
downloadArchive = "download-archive"

outputFormat :: String
outputFormat = "%(uploader)s-%(upload_date)s-%(title)s.mp4"

otherOptions :: [String]
otherOptions = ["--ignore-errors"]

-- True if the config selects all channels
selectsAll :: Config -> Bool
selectsAll config = isNothing n && isNothing t
  where
    n = matchName config
    t = matchTag config

-- Is first string a proper (case-insensitive) substring of the second
isSubstring :: String -> String -> Bool
isSubstring [] _      = False
isSubstring pattern s = isInfixOf (map toLower pattern) (map toLower s)

-- Is there a match between the name in the config and the name of the channel
hasNameMatch :: Config -> Channel -> Bool
hasNameMatch config channel = hasNameMatch' (matchName config) (name channel)
    where
      hasNameMatch' :: Maybe String -> Maybe String -> Bool
      hasNameMatch' (Just pattern) (Just s) = isSubstring pattern s
      hasNameMatch' _ _                     = False

-- Is there a match between the tag name in the config and a tag in the channel
hasTagMatch :: Config -> Channel -> Bool
hasTagMatch config channel = hasTagMatch' (matchTag config) (tags channel)
    where
      hasTagMatch' :: Maybe String -> Maybe [String] -> Bool
      hasTagMatch' (Just pattern) (Just xs) = any (isSubstring pattern) xs
      hasTagMatch' _ _                      = False

-- If the option is present return it with the given string, otherwise empty string
expandOption :: String -> Maybe String -> [String]
expandOption s (Just t) = [s, t]
expandOption _ Nothing  = []

-- Build the channel's arguement list for youtube-dl
buildArgs :: Config -> Channel -> ([String], Bool)
buildArgs config channel = (args, active)
  where
    active :: Bool
    active =
      (disabled channel /= Just True) &&
      (selectsAll config || hasNameMatch config channel ||  hasTagMatch config channel)
    args :: [String]
    args =
      (if (simulate config) then ["--simulate"] else []) ++
      standardArgs ++
      otherOptions ++
      expandOption "--match-title" (match channel) ++
      expandOption "--reject-title" (reject channel) ++
      [baseUrl ++ (url channel)]
    standardArgs :: [String]
    standardArgs =
      [ "--format"
      , fromMaybe (defaultFormat config) (format channel)
      , "--output"
      , outputFormat
      , "--download-archive"
      , downloadArchive
      , "--playlist-items"
      , "1-" ++ (show (fromMaybe (maxVideos config) (Channels.max channel)))
      ]

download :: Config -> Channel -> IO ExitCode
download config channel = do
  let (args, active) = buildArgs config channel
  let n = (fromMaybe "Anonymous" (name channel)) ++ ": "
  when active $ do
    putStrLn . unwords $ n : youtube_dl : args
    hFlush stdout
  if (active && not (echo config)) then rawSystem youtube_dl args else return ExitSuccess

downloadAll :: Config -> [Channel] -> IO ()
downloadAll config = go
    where
        go :: [Channel] -> IO ()
        go (c : cs) = do
          exitCode <- download config c
          case exitCode of
            ExitSuccess -> go cs
            ExitFailure x -> putStrLn $ "Failed with code " ++ show x