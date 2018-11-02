{-# LANGUAGE DeriveGeneric #-}

module Channels
  ( Channel
  , readChannels
  ) where

import Data.Aeson (eitherDecode, FromJSON)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import System.FilePath (FilePath)

defaultChannelFile :: FilePath
defaultChannelFile = "test_channels.json"

data Channel = Channel {
  url :: String,
  name :: Maybe String,
  format :: Maybe String,
  tags :: Maybe [String],
  max :: Maybe Int,
  match :: Maybe String,
  reject :: Maybe String,
  disabled :: Maybe Bool
} deriving (Generic, Show)

instance FromJSON Channel

readChannels :: Maybe FilePath -> IO (Either String [Channel])
readChannels p =  eitherDecode <$> B.readFile (fromMaybe defaultChannelFile p)