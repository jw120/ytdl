-- Data type and reader for the list of channels

{-# LANGUAGE DeriveGeneric #-}

module Channels
  ( Channel(..)
  , readChannels
  ) where

import           Data.Aeson           (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import           Data.Maybe           (fromMaybe)
import           GHC.Generics
import           System.Directory     (getHomeDirectory)
import           System.FilePath      (FilePath)

defaultChannelFile :: FilePath
defaultChannelFile = ".ytdl.json"

data Channel = Channel {
  url      :: String,
  name     :: Maybe String,
  format   :: Maybe String,
  tags     :: Maybe [String],
  max      :: Maybe Int,
  match    :: Maybe String,
  reject   :: Maybe String,
  disabled :: Maybe Bool
} deriving (Generic, Show)

instance FromJSON Channel

readChannels :: Maybe FilePath -> IO (Either String [Channel])
readChannels p =  do
  homeDir <- getHomeDirectory
  let def = homeDir ++ "/" ++ defaultChannelFile
  eitherDecode <$> B.readFile (fromMaybe def p)
