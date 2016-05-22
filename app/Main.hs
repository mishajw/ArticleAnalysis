module Main where

import System.Environment (getArgs)

import Lib

main = do
  args <- getArgs
  case args of
    dir : amount : [] -> someFunc dir (read amount)
    _ -> putStrLn "Incorrect use, need <directory> <amount of groups>"

