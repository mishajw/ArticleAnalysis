module Main where

import System.IO (readFile)
import WordCounting (clusterTexts)
import WordCountTypes (UncountedText(..), WordCount(..))

files = map (\i -> "test" ++ show i ++ ".txt") [1..4]

main = do
  contents <- mapM readFile files

  let labeledContent = zip files contents
  let uts = map (\(f, s) -> UncountedText { utTitle=f, utText=s }) labeledContent
  let clusters = clusterTexts uts
  
  print $ map (map wcTitle) clusters

