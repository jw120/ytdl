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

import           Config              (Config(..))
import           Tilde                 (tildeExpand)

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

readChannels :: Config -> IO (Either String [Channel])
readChannels config =  do
  f <- tildeExpand $ channelsFile config
  eitherDecode <$> B.readFile f
