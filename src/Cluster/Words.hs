module Cluster.Words (clusterTexts) where

import Cluster.WordTypes
import qualified Cluster.Kmeans

import qualified Data.List as L

-- | Run the clustering on a list of text
clusterTexts :: Int -> [UncountedText] -> [[WordCount]]
clusterTexts i ss =
  let wcs = map mkWordCount ss in
  Cluster.Kmeans.kmeans i wcs

instance Cluster.Kmeans.Clusterable WordCount where
  cmp c1 c2 =
    let prepare = wordCountToCollection . sortWordCount in
    let (c1', c2') = (prepare c1, prepare c2) in
    let merged = mergeWordCounts c1' c2' in
    fromIntegral $ diff merged where

      -- | Get the difference between a word count collection
      diff :: WordCountCollection -> Int
      diff (WordCountCollection wcc) =
        let cs = map snd wcc in
        sum $ map (\is -> (maximum is) - (minimum is)) cs

      -- | Sort a word count alphabetically
      sortWordCount :: WordCount -> WordCount
      sortWordCount wc = wc {
        wcCounts = L.sortBy (\(s, _) (s', _) -> compare s s') (wcCounts wc)
      }

  avg [] = WordCount { wcTitle="", wcCounts=[] }
  avg wcs =
    let wccs = map wordCountToCollection wcs in
    let WordCountCollection merged = foldl1 mergeWordCounts wccs in
    WordCount {
      wcTitle = ""
    , wcCounts = map (\(s, is) -> (s, (sum is) `div` (length is))) merged
    }

mergeWordCounts :: WordCountCollection -> WordCountCollection -> WordCountCollection
mergeWordCounts (WordCountCollection wc1) (WordCountCollection wc2) =
  WordCountCollection $ mergeCounts wc1 wc2

-- | Merge two word counts, must be sorted alphabetically
mergeCounts ::  [(String, [Int])] -> [(String, [Int])] -> [(String, [Int])]
mergeCounts [] [] = []
mergeCounts cs@(_:_) [] = map (\(s, is) -> (s, 0 : is)) cs
mergeCounts [] cs@(_:_) = map (\(s, is) -> (s, 0 : is)) cs
mergeCounts c1@((s1, i1) : cs1) c2@((s2, i2) : cs2) =
  if s1 == s2
  then (s1, i1 ++ i2) : mergeCounts cs1 cs2
  else
    if s1 < s2
    then (s1, 0 : i1) : mergeCounts cs1 c2
    else (s2, 0 : i2) : mergeCounts c1 cs2

