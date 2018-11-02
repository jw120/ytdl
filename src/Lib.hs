module Lib
    ( someFunc
    ) where

someFunc :: (String, String, String, Bool) -> IO ()
someFunc x = do
    putStrLn "someFunc"
    putStrLn (show x)
