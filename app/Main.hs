module Main where

import           Options.Applicative
import           System.Directory    (setCurrentDirectory)

import           Channels            (readChannels)
import           Config              (Config(..), readConfig)
import           CLOpts              (CLOpts(..), parser)
import           Download            (downloadAll)
import           Tilde               (tildeExpand)

opts :: ParserInfo CLOpts
opts = info parser
  ( fullDesc
  <> progDesc "Downloads recent videos from youtube channels described in the channels file")

main :: IO ()
main = do
  clopts <- execParser opts
  configResult <- readConfig clopts
  case configResult of
    Left configErr -> putStrLn configErr
    Right config -> do
      case (outputDir config) of
        Just dir -> do
          dir' <- tildeExpand dir
          setCurrentDirectory dir'
        Nothing -> return ()
      channelsResult <- readChannels config
      case channelsResult of
        Left channelsErr       -> putStrLn channelsErr
        Right channels -> downloadAll config channels
