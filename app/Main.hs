module Main where

import Options.Applicative

import Channels (readChannels)
import Config (Config(..), parser)
import Download (download)

opts :: ParserInfo Config
opts = info parser
  ( fullDesc
  <> progDesc "Downloads recent videos from youtube channels described in the channels file")

main :: IO ()
main = do
  config <- execParser opts
  channelsResult <- readChannels (channelsFile config)
  case channelsResult of
    Left err -> putStrLn err
    Right channels -> mapM_ (download config) channels
