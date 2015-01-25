module Tsp.Opt2Spec (main, spec) where

import Test.Hspec
import Tsp.Common
import Tsp.Opt2

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "possible swaps" $
    it "should generate all possible swaps" $
      possibleSwaps 5 `shouldBe` [(0, 1), (0, 2), (1, 2), (1, 3), (2, 3), (2, 4), (3, 4)]

  describe "swap" $
    it "should swap 2 edges in a solution" $ do
      swap [0, 1, 2, 3, 4] (0, 2) ` shouldBe` [2, 1, 0, 3, 4]
      swap [0, 2, 3, 1, 4] (0, 3) `shouldBe` [1, 3, 2, 0, 4]
      swap [0, 1, 2, 3, 4] (1, 3) `shouldBe` [0, 3, 2, 1, 4]
      swap [0, 1, 2, 3, 4] (1, 4) `shouldBe` [0, 4, 3, 2, 1]
      swap [0, 1, 2, 3, 4] (2, 4) `shouldBe` [0, 1, 4, 3, 2]

  describe "improvement" $
    it "should generate neighbour correctly" $ do
      let distances = distancesMap [(0, 0), (0, 2), (2, 0), (2, 2)]
      let s = [0, 3, 1, 2]
      let delta = improvement distances s (1, 2)
      let expectedDelta = 8 - (4 + 2 * sqrt(8))
      delta `shouldSatisfy` (<0.0001) . abs . (subtract expectedDelta)



