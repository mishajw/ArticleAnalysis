module WordCountClustering where

import qualified Clustering
import WordCount (wordCount)

import Data.List

runOnText :: [String] -> [[[(String, Int)]]]
runOnText ss =
  let wcs = map wordCount ss in
  Clustering.kmeans cmpWordCount avgWordCount 3 wcs
  
cmpWordCount :: [(String, Int)] -> [(String, Int)] -> Int
cmpWordCount c1 c2 =
  let c1' = map (\(s, i) -> (s, [i])) (sortWordCount c1) in
  let c2' = map (\(s, i) -> (s, [i])) (sortWordCount c2) in
  let merged = mergeWordCounts c1' c2' in
  diff merged where
    diff :: [(String, [Int])] -> Int
    diff ((_, (i1 : i2 : _)) : rest) =
      (abs (i1 - i2)) + (diff rest)
    diff [] = 0
    diff (_ : rest) = diff rest

    sortWordCount :: [(String, a)] -> [(String, a)]
    sortWordCount = sortBy (\(s, _) (s', _) -> compare s s')

avgWordCount :: [[(String, Int)]] -> [(String, Int)]
avgWordCount wcs =
  let wcs' = map (map (\(s, i) -> (s, [i]))) wcs in
  let merged = foldl mergeWordCounts [] wcs' in
  map (\(s, is) -> (s, sum is)) merged

mergeWordCounts :: [(String, [Int])] -> [(String, [Int])] -> [(String, [Int])]
mergeWordCounts [] [] = []
mergeWordCounts cs@(_:_) [] = cs
mergeWordCounts [] cs@(_:_) = cs
mergeWordCounts c1@((s1, i1) : cs1) c2@((s2, i2) : cs2) =
  if s1 == s2
  then (s1, i1 ++ i2) : mergeWordCounts cs1 cs2
  else
    if s1 < s2
    then (s1, i1) : mergeWordCounts cs1 c2
    else (s2, i2) : mergeWordCounts c1 cs2

