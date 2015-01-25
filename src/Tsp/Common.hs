module Tsp.Common where

import Data.Ix
import qualified Data.Map.Strict as Map

type Edge = (Double, Double)
type Distances = Map.Map (Int, Int) Double
type Solution = [Int]
type Neighbour = (Solution, Double)

distance :: Edge -> Edge -> Double
distance (x1, y1) (x2, y2)
    | x1 == x2 && y1 == y2 = 1000000000
    | otherwise = sqrt (square(x2 - x1) + square(y2 - y1))
    where square n = n * n

distancesMap :: [Edge] -> Distances
distancesMap e = Map.fromList [((i, j), distance (e !! i) (e !! j)) | (i, j) <- range bounds, i /= j]
    where l = length e - 1
          bounds = ((0, 0), (l, l))

fitness :: Distances -> Solution -> Double
fitness _ [] = 0.0
fitness distances solution = sum edgeDistances
    where edges = zip solution (tail solution ++ [head solution])
          edgeDistances = Prelude.map (distances Map.!) edges

toEdge :: [String] -> (Double, Double)
toEdge [x, y] = (read x :: Double, read y :: Double)

parse :: [String] -> [Edge]
parse lines = foldr edges [] $ tail lines
    where pair s = toEdge $ words s
          toEdge [x, y] = (read x :: Double, read y :: Double)
          edges line acc = pair line : acc
