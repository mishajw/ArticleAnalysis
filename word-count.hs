module WordCount where

import Data.List

wordCount :: String -> [(String, Int)]
wordCount full =
  let split = splitOn ' ' full in
  count split 

-- Split a a list by a delimiter
splitOn :: (Eq a) => a -> [a]  -> [[a]]
splitOn _ [] = []
splitOn del (a : as) =
  if a == del
  then ([] : splitOn del as)
  else case (splitOn del as) of
    [] -> [[a]]
    (as' : ass') -> (a : as') : ass'

-- Count the repeated values in an ordered list
count :: (Eq a, Ord a) => [a] -> [(a, Int)]
count as = count' (sort as) 0 where
  count' (hd1 : tl1@(hd2 : tl2)) acc =
    if hd1 == hd2
    then count' tl1 (acc + 1)
    else (hd1, acc + 1) : (count' tl1 0)
  count' (a : as) acc = [(a, acc + 1)]
  count' [] _ = []

