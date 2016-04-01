module Clustering where

import Data.List

-- |K-means clustering.
-- |Takes Comparator, Averager, amount of clusters, and the list to cluster.
kmeans :: (Eq a, Ord b, Num b) => (a -> a -> b) -> ([a] -> a) -> Int -> [a] -> [[a]]
kmeans cmp avg n as =
  kmeans' cmp avg as (take n as) 100 where

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
    map (\(p, c) -> c) (clusterTo' as cs cmp) where

    clusterTo' :: (Eq a, Ord b, Num b) => [a] -> [a] -> (a -> a -> b) -> [(a, [a])]
    clusterTo' [] cs cmp = map (\c -> (c, [])) cs
    clusterTo' (a : as) cs cmp =
      -- Get the rest
      let rest = clusterTo' as cs cmp in
      -- Sort by distance to node and get the head
      let ((centre, children) : rs) = sortBy (\(c, _) (c', _) -> compare (cmp a c) (cmp a c')) rest in
      -- Append current to head
      (centre, a : children) : rs

kmeansTest = kmeans cmpInt avgInt 4 [1, 1, 3, 3, 10, 100, 10, 10, 200, 200, 200, 1, 1, 1, 1, 1] where
  cmpInt :: Int -> Int -> Int
  cmpInt a b = abs (a - b)
  avgInt :: [Int] -> Int
  avgInt as@(_:_) = ((sum as) `div` (length as)) :: Int
  avgInt _ = 0

