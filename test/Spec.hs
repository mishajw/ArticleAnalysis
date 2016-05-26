import Test.Hspec
import Test.QuickCheck

import Data.List (sort)
import GHC.Exts (sortWith)

import Lib

main = hspec $ do
  describe "Clustering" $ do
    it "can cluster basic integers correctly" $ do
      (orderIntCluster $ clusterInts 2 [3, 2, 1, 7, 8, 9]) `shouldBe` [[1, 2, 3], [7, 8, 9]]

clusterInts :: Int -> [Int] -> [[Int]]
clusterInts = kmeans cmpInt avgInt

orderIntCluster :: [[Int]] -> [[Int]]
orderIntCluster is = sortWith sum $ map sort is

cmpInt :: Int -> Int -> Int
cmpInt a b = abs (a - b)

avgInt :: [Int] -> Int
avgInt is = sum is `div` length is

