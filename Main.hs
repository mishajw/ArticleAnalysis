module Main where

import System.IO
import System.Environment (getArgs)
import System.Directory (getDirectoryContents)

import WordCounting (clusterTexts)
import WordCountTypes (UncountedText(..), WordCount(..))

-- files = map (\i -> "test" ++ show i ++ ".txt") [1..4]

main = do
  args <- getArgs
  case args of
    dir : amount : [] -> run dir (read amount)
    _ -> putStrLn "Incorrect use, need <directory> <amount of groups>"

run :: String -> Int -> IO ()
run dir amount = do
  files <- getFileNames dir
  uts <- mapM getUncountedText files

  let clusters = clusterTexts 2 uts
  
  print $ map (map wcTitle) clusters

getFileNames :: FilePath -> IO [FilePath]
getFileNames dir = do 
  files <- getDirectoryContents dir 
  return $ (map (dir ++) . filter (\(c:_) -> c /= '.')) files

getUncountedText :: FilePath -> IO UncountedText
getUncountedText path = do
  handle <- openFile path ReadMode
  hSetEncoding handle char8
  contents <- hGetContents handle

  let ut = UncountedText {
    utTitle = path
  , utText = contents
  }

  -- hClose handle

  return ut

