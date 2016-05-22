module Lib
    ( someFunc
    ) where

import Run

someFunc :: String -> Int -> IO ()
someFunc = run
