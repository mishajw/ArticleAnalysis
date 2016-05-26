import Test.Hspec
import Test.QuickCheck

import Data.List (sort)
import GHC.Exts (sortWith)
import System.IO

import Lib

instance Clusterable Int where
  cmp a b = fromIntegral . toInteger $ abs (a - b)
  avg is = sum is `div` length is

main = hspec $ do
  describe "Cluster.Kmeans" $
    it "can cluster basic integers correctly" $
      orderCluster (kmeans 2 ([3, 2, 1, 7, 8, 9] :: [Int])) `shouldBe` [[1, 2, 3], [7, 8, 9]]
  describe "Cluster.Words" $
    it "can cluster words" $ do
      let clusters = clusterTexts 2 [ UncountedText "a1" "a aa aaa a"
                                    , UncountedText "a2" "aa a a, aa, aa"
                                    , UncountedText "a3" "a, a, aa,b, a,aa"
                                    , UncountedText "b1" "bbb b b bb, bb"
                                    , UncountedText "b2" "bb,b, a, bbb, b"
                                    , UncountedText "b3" "b, bb, bbb, bb, b b" ]

      all allEqual $ map (map $ head . wcTitle) clusters

orderCluster :: (Num a, Ord a) => [[a]] -> [[a]]
orderCluster is = sortWith sum $ map sort is

allEqual :: Eq a => [a] -> Bool
allEqual (h : t) = all (== h) t

