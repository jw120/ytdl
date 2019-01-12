-- Tilde expand a leading ~ into the user's home directory only

module Tilde (tildeExpand) where

import System.Directory (getHomeDirectory)

tildeExpand :: FilePath -> IO FilePath
tildeExpand ('~' : r) = do
  homeDir <- getHomeDirectory
  return $ homeDir ++ r
tildeExpand p = return p
