module Cluster.Words (
  clusterTexts
, UncountedText(..)
, WordCount(..)
, mkWordCount
) where

import Cluster.Kmeans

import qualified Data.List as L
import Data.Char (toLower, isAlphaNum, isSpace)
import Control.Arrow ((&&&), second)

-- | Run the clustering on a list of text
clusterTexts :: (Int -> [WordCount] -> [[WordCount]]) -> Int -> [UncountedText] -> [[WordCount]]
clusterTexts alg i ss =
  let wcs = map mkWordCount ss in
  alg i wcs

-- Uncounted text
data UncountedText = UncountedText {
  utTitle :: String
, utText :: String
} deriving (Show)

-- | A word count for a piece of text
data WordCount = WordCount {
  wcTitle :: String
, wcCounts :: [(String, Int)]
} deriving (Show, Eq)

-- | A collection of word counts
data WCCollection = WCCollection [(String, [Int])] deriving (Show)

instance Clusterable WordCount where
  cmp c1 c2 =
    let prepare = mkWCCollection . sortWordCount in
    let (c1', c2') = (prepare c1, prepare c2) in
    let merged = mergeWordCounts c1' c2' in
    fromIntegral $ diff merged where

      -- | Get the difference between a word count collection
      diff :: WCCollection -> Int
      diff (WCCollection wcc) =
        let cs = map snd wcc in
        sum $ map (\is -> maximum is - minimum is) cs

      -- | Sort a word count alphabetically
      sortWordCount :: WordCount -> WordCount
      sortWordCount wc = wc {
        wcCounts = L.sortBy (\(s, _) (s', _) -> compare s s') (wcCounts wc)
      }

  avg [] = WordCount { wcTitle="", wcCounts=[] }
  avg wcs =
    let wccs = map mkWCCollection wcs in
    let WCCollection merged = foldl1 mergeWordCounts wccs in
    WordCount {
      wcTitle = ""
    , wcCounts = map (\(s, is) -> (s, sum is `div` length is)) merged
    }

-- | Convert from WordCount to WCCollection
mkWCCollection :: WordCount -> WCCollection
mkWCCollection wc =
  WCCollection $ map (\(s, i) -> (s, [i])) (wcCounts wc)

-- | Make a word count from a string
mkWordCount :: UncountedText -> WordCount
mkWordCount ut =
  WordCount {
    wcTitle = utTitle ut
  , wcCounts = (
      map (head &&& length) .
      L.group . L.sort . wordsOperator . words .
      map toLower . filter (\c -> isAlphaNum c || isSpace c)
    ) $ utText ut
  }

wordsOperator :: [String] -> [String]
wordsOperator = getNouns . map (map toLower) where
  getNouns :: [String] -> [String]
  getNouns (w : w' : ws) =
    if w `elem` dets
      then w' : getNouns ws
      else getNouns ws
  getNouns _ = []

  dets = ["the", "a", "an", "those", "these", "this"]

-- | Combine two word count collections
mergeWordCounts :: WCCollection -> WCCollection -> WCCollection
mergeWordCounts (WCCollection wc1) (WCCollection wc2) =
  WCCollection $ mergeCounts wc1 wc2

-- | Merge two word counts, must be sorted alphabetically
mergeCounts ::  [(String, [Int])] -> [(String, [Int])] -> [(String, [Int])]
mergeCounts [] [] = []
mergeCounts cs@(_:_) [] = map (second ((:) 0)) cs
mergeCounts [] cs@(_:_) = map (second ((:) 0)) cs
mergeCounts c1@((s1, i1) : cs1) c2@((s2, i2) : cs2)
  | s1 == s2   = (s1, i1 ++ i2) : mergeCounts cs1 cs2
  | s1 < s2    = (s1, 0 : i1)   : mergeCounts cs1 c2
  | otherwise  = (s2, 0 : i2)   : mergeCounts c1 cs2

