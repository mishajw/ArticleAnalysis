module Cluster.WordTypes (
  UncountedText(..)
, WordCount(..)
, WordCountCollection(..)
, mkWordCount
, wordCountToCollection
) where

import Data.List
import Data.Char (toLower, isAlphaNum, isSpace)

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
data WordCountCollection = WordCountCollection [(String, [Int])] deriving (Show)

-- | Convert from WordCount to WordCountCollection
wordCountToCollection :: WordCount -> WordCountCollection
wordCountToCollection wc =
  WordCountCollection $ map (\(s, i) -> (s, [i])) (wcCounts wc)

-- | Make a word count from a string
mkWordCount :: UncountedText -> WordCount
mkWordCount ut =
  WordCount {
    wcTitle = utTitle ut
  , wcCounts = (
      map (\xs -> (head xs, length xs)) .
      group . sort . words .
      map toLower . filter (\c -> (isAlphaNum c) || (isSpace c))
    ) $ utText ut
  }

