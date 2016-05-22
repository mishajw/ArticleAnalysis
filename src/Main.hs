module Main where

import System.IO
import System.Environment (getArgs)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))

import WordCounting (clusterTexts)
import WordCountTypes (UncountedText(..), WordCount(..))

main = do
  args <- getArgs
  case args of
    dir : amount : [] -> run dir (read amount)
    _ -> putStrLn "Incorrect use, need <directory> <amount of groups>"

run :: String -> Int -> IO ()
run dir amount = do
  putStrLn $ "Putting files from " ++ dir ++ " into " ++ show amount ++ " clusters"

  files <- getFileNames dir
  uts <- mapM getUncountedText files

  let clusters = clusterTexts amount uts
  
  mapM_ (print . map wcTitle) clusters

getFileNames :: FilePath -> IO [FilePath]
getFileNames dir = do 
  files <- getDirectoryContents dir 
  return $ (map (dir ++) . filter ((/= '.') . head)) files

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

