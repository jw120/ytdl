module Lib
    ( someFunc
    ) where

import qualified ProgramOptions

someFunc :: ProgramOptions.Config -> IO ()
someFunc cfg = do
    putStrLn "someFunc"
    print cfg
