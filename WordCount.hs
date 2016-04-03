module WordCount where

import Data.List

-- |Get a word count of a string
wordCount :: String -> [(String, Int)]
wordCount = map (\xs -> (head xs, length xs)) . group . sort . words

