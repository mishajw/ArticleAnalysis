module WordCountTypes (
  UncountedText(..)
, WordCount(..)
, WordCountCollection(..)
, mkWordCount
, wordCountToCollection
) where

import Data.List
import Data.Char (toLower, isAlphaNum, isSpace)

data UncountedText = UncountedText {
  utTitle :: String
, utText :: String
} deriving (Show)

data WordCount = WordCount {
  wcTitle :: String
, wcCounts :: [(String, Int)]
} deriving (Show, Eq)

data WordCountCollection = WordCountCollection [(String, [Int])] deriving (Show)

wordCountToCollection :: WordCount -> WordCountCollection
wordCountToCollection wc =
  WordCountCollection $ map (\(s, i) -> (s, [i])) (wcCounts wc)

-- |Get a word count of a string
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

