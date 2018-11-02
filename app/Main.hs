module Main where

import Options.Applicative

import Channels
import Lib
import qualified ProgramOptions

opts :: ParserInfo ProgramOptions.Config
opts = info ProgramOptions.parser
  ( fullDesc
  <> progDesc "Downloads recent videos from youtube channels described in the channels file")

main :: IO ()
main = do
  config <- execParser opts
  channelsResult <- readChannels (ProgramOptions.channelsFile config)
  case channelsResult of
    Left err -> putStrLn err
    Right channels -> print channels
  someFunc config
