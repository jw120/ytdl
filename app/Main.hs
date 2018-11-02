module Main where

import Options.Applicative

import Lib

data CommandLineOptions = CommandLineOptions {
  channelsFile :: String,
  name :: String,
  tag :: String,
  simulate :: Bool
}

parser :: Parser CommandLineOptions
parser = CommandLineOptions <$>
  strOption
    ( long "channels"
    <> metavar "FILENAME"
    <> help "override channels.json as default list of channels"
    ) <*>
  strOption
    ( long "name"
    <> metavar "STRING"
    <> help "select only channels with names that include the STRING (case-insensitive)"
    ) <*>
  strOption
    ( long "tag"
    <> metavar "STRING"
    <> help "select only channels with a tag that include the STRING (case-insensitive)"
    ) <*>
  switch
    ( long "simulate"
    <> short 's'
    <> help "Enable simulation mode")


opts :: ParserInfo CommandLineOptions
opts = info parser
  ( fullDesc
  <> progDesc "Download youtube channels"
  <> header "ytdl - a test for optparse-applicative" )

main :: IO ()
main = do
  options <- execParser opts
  someFunc (channelsFile options, name options, tag options, simulate options)
