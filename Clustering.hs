module Clustering where

import Data.List

defaultIterations = 100

-- |K-means clustering.
-- |Takes Comparator, Averager, amount of clusters, and the list to cluster.
kmeans :: (Eq a, Ord b, Num b) => (a -> a -> b) -> ([a] -> a) -> Int -> [a] -> [[a]]
kmeans cmp avg n as =
  kmeans' cmp avg as (take n as) defaultIterations where

  -- |Also takes the current clusters, and iterations to go
  kmeans' :: (Eq a, Ord b, Num b) => (a -> a -> b) -> ([a] -> a) -> [a] -> [a] -> Int -> [[a]]
  kmeans' cmp avg as cs 0 =
    clusterTo as cs cmp
  kmeans' cmp avg as cs iteration =
    let clusters = clusterTo as cs cmp in
    let newCenters = map avg clusters in
    kmeans' cmp avg as newCenters (iteration - 1)

  clusterTo :: (Eq a, Ord b, Num b) => [a] -> [a] -> (a -> a -> b) -> [[a]]
  clusterTo as cs cmp =
    map snd (clusterTo' as cs cmp) where

    clusterTo' :: (Eq a, Ord b, Num b) => [a] -> [a] -> (a -> a -> b) -> [(a, [a])]
    clusterTo' [] cs cmp = map (\c -> (c, [])) cs
    clusterTo' (a : as) cs cmp =
      -- Get the rest
      let rest = clusterTo' as cs cmp in
      -- Sort by distance to node and get the head
      let ((centre, children) : rs) = sortBy (\(c, _) (c', _) -> compare (cmp a c) (cmp a c')) rest in
      -- Append current to head
      (centre, a : children) : rs

testKmeans = kmeans cmp avg 4 [1,2,3,7,8,9] where
  
  cmp :: Int -> Int -> Int
  cmp a b = abs (a - b)

  avg :: [Int] -> Int
  avg is = (sum is) `div` (length is)

