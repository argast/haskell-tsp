module Main where

import Test.Hspec
import Tsp

main :: IO ()
main = hspec $ do

  describe "distance" $ do

    it "should be euclidean distance between 2 points" $
      distance (0, 0) (0, 1) `shouldBe` 1.0

    it "should be 1000000000 for distance between same point" $
      distance (0, 0) (0, 0) `shouldBe` 1000000000

  describe "fitness should be " $ do

     let fitness' = fitness $ distancesMap [(0, 0), (2, 0), (2, 2)]
     it "0 for empty solution" $
       fitness' [] `shouldBe` 0

     it "double distance between edges for single edge solution" $
       fitness' [0, 1] `shouldBe` 4.0

     it "sum of distancess between edges for multi edge solution" $
       fitness' [0, 1, 2] `shouldBe` (2.0 + 2.0 + sqrt(8))


  describe "possible swaps" $
    it "should generate all possible swaps" $
      possibleSwaps 5 `shouldBe` [(0, 1), (0, 2), (1, 2), (1, 3), (2, 3), (2, 4), (3, 4)]

  describe "swap" $
    it "should swap 2 edges in a solution" $ do
      swap [0, 1, 2, 3, 4] 0 2 `shouldBe` [2, 1, 0, 3, 4]
      swap [0, 2, 3, 1, 4] 0 3 `shouldBe` [1, 3, 2, 0, 4]
      swap [0, 1, 2, 3, 4] 1 3 `shouldBe` [0, 3, 2, 1, 4]
      swap [0, 1, 2, 3, 4] 1 4 `shouldBe` [0, 4, 3, 2, 1]
      swap [0, 1, 2, 3, 4] 2 4 `shouldBe` [0, 1, 4, 3, 2]

  describe "neighbour" $
    it "should generate neighbour correctly" $ do
      let distances = distancesMap [(0, 0), (0, 2), (2, 0), (2, 2)]
      let s = [0, 3, 1, 2]
      let (newSolution, delta) = neighbour distances s (1, 2)
      newSolution `shouldBe` [0, 1, 3, 2]
      let expectedDelta = 8 - (4 + 2 * sqrt(8))
      delta `shouldSatisfy` (<0.0001) . abs . (subtract expectedDelta)



