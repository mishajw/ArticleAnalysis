module Main where

import System.IO (readFile)
import WordCountClustering (runOnText, wordCount)

files = map (\i -> "test" ++ show i ++ ".txt") [1..4]

main = do
  contents <- mapM readFile files
  print $ runOnText contents

