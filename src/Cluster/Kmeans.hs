module Cluster.Kmeans (kmeans, Clusterable(..)) where

import Data.List

defaultIterations = 10

class Clusterable a where
  avg :: [a] -> a
  cmp :: a -> a -> Float

-- | K-means clustering.
-- | Takes Comparator, Averager, amount of clusters, and the list to cluster.
kmeans :: Clusterable a => Int -> [a] -> [[a]]
kmeans n as =
  kmeans' as (take n as) defaultIterations where

  -- | Also takes the current clusters, and iterations to go
  kmeans' :: Clusterable a => [a] -> [a] -> Int -> [[a]]
  kmeans' as cs 0 =
    clusterTo as cs
  kmeans' as cs iteration =
    let clusters = clusterTo as cs in
    let newCenters = map avg clusters in
    kmeans' as newCenters (iteration - 1)

  clusterTo :: Clusterable a => [a] -> [a] -> [[a]]
  clusterTo as cs =
    map snd (clusterTo' as cs) where

    clusterTo' :: Clusterable a => [a] -> [a] -> [(a, [a])]
    clusterTo' [] cs = map (\c -> (c, [])) cs
    clusterTo' (a : as) cs =
      -- Get the rest
      let rest = clusterTo' as cs in
      -- Sort by distance to node and get the head
      let ((centre, children) : rs) = sortBy (\(c, _) (c', _) -> compare (cmp a c) (cmp a c')) rest in
      -- Append current to head
      (centre, a : children) : rs

