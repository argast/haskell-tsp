module Tsp.CommonSpec (main, spec) where

import Test.Hspec
import Tsp.Common

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

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




