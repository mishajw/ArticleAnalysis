import Test.Hspec
import Test.QuickCheck

import Data.List (sort)
import GHC.Exts (sortWith)

import Lib

main = hspec $ do
  describe "Clustering" $ do
    it "cluster basic integers correctly" $ do
      let cs = kmeans cmpInt avgInt 2 [3, 2, 1, 7, 8, 9]
      let normalised = sortWith sum $ map sort cs

      normalised `shouldBe` [[1, 2, 3], [7, 8, 9]]

cmpInt :: Int -> Int -> Int
cmpInt a b = abs (a - b)

avgInt :: [Int] -> Int
avgInt is = sum is `div` length is

