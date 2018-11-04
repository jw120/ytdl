-- Data type and parser for configuration from command line options

module Config
    ( Config(..)
    , parser
    ) where

import Options.Applicative

data Config = Config {
  channelsFile :: Maybe String,
  matchName :: Maybe String,
  matchTag :: Maybe String,
  simulate :: Bool,
  echo :: Bool
} deriving (Show)

parser :: Parser Config
parser = Config <$>
  optional (strOption
    ( long "channels"
    <> metavar "FILENAME"
    <> help "override channels.json as default list of channels")) <*>
  optional (strOption
    ( long "name"
    <> metavar "STRING"
    <> help "select only channels with names that include the STRING (case-insensitive)")) <*>
  optional (strOption
    ( long "tag"
    <> metavar "STRING"
    <> help "select only channels with a tag that include the STRING (case-insensitive)")) <*>
  switch
    ( long "simulate"
    <> short 's'
    <> help "Pass --simulate option to youtube-dl") <*>
  switch
    ( long "echo"
    <> short 'e'
    <> help "Echo commands instead of running them")
