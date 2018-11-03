module Download
  ( download
  ) where

import Channels
import qualified ProgramOptions

download :: ProgramOptions.Config -> Channel -> IO ()
download _ channel = putStrLn (url channel)