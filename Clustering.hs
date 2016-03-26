module Clustering where

-- |K-means clustering.
-- |Takes Comparator, Averager, amount of clusters, and the list to cluster.
kmeans :: (Num b) => (a -> a -> b) -> ([a] -> a) -> Int -> [a] -> [[a]]
kmeans cmp avg n as =
  kmeans' cmp avg as (take n as) 10 where

  -- |Also takes the current clusters, and iterations to go
  kmeans' :: (Num b) => (a -> a -> b) -> ([a] -> a) -> [a] -> [a] -> Int -> [[a]]
  kmeans' cmp avg as cs 0 =
    clusterTo as cs cmp
  kmeans' cmp avg as cs iteration =
    let clusters = clusterTo as cs cmp in
    let newCenters = map avg clusters in
    kmeans' cmp avg as newCenters (iteration - 1)

  clusterTo :: (Num b) => [a] -> [a] -> (a -> a -> b) -> [[a]]
  clusterTo (a : as) cs cmp = []

kmeansTest = kmeans cmp avg 3 [1, 1, 2, 4, 4, 4, 6, 7, 8] where
  cmp :: Integer -> Integer -> Integer
  cmp a b = a - b
  avg :: [Integer] -> Integer
  avg as = ((sum as) / ((length as) :: Integer)) :: Integer

