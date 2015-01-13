module Tsp where

import Data.Ix
import Data.List
import qualified Data.Map.Strict as Map
import Data.Array.IO
import Control.Monad

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
          edgeDistances = {-# SCC "aa" #-} Prelude.map (distances Map.!) edges

toEdge :: [String] -> (Double, Double)
toEdge (x : y : []) = (read x :: Double, read y :: Double)

parse :: [String] -> [Edge]
parse lines = foldr edges [] $ tail lines
    where pair s = toEdge $ words s
          toEdge (x : y : []) = (read x :: Double, read y :: Double)
          edges line acc = pair line : acc

possibleSwaps :: Int -> [(Int, Int)]
possibleSwaps size = [(i, j) | i <- [0..size - 1], j <- [i + 1..size - 1], i /= (j + 1) `mod` size, j - i < size - 2]

swap :: Solution -> Int -> Int -> Solution
swap solution i1 i2 = beginning ++ middle ++ end
    where beginning = take from solution
          middle = (reverse . take (to - from + 1) . drop from) solution
          end = drop (to + 1) solution
          from = min i1 i2
          to = max i1 i2

swap' :: (IOArray Int Int) -> Int -> Int -> IO (IOArray Int Int)
swap' arr i j = do
    forM_ [0..((j - i) `mod` 2)] $ \k -> do
        vi <- readArray arr (i + k)
        vj <- readArray arr (j - k)
        writeArray arr (i + k) vj
        writeArray arr (j - k) vi
    return arr


neighbour :: Distances -> Solution -> (Int, Int) -> Neighbour
neighbour distances solution (i1, i2) = (newSolution, newDistance - oldDistance)
    where newSolution = swap solution i1 i2
          size = length solution
          ind i = (i + size) `mod` size
          edgeByIndex i = solution !! (ind i)
          distanceByIndex ind1 ind2 = distances Map.! (edgeByIndex ind1, edgeByIndex ind2)
          oldDistance = distanceByIndex i1 (i1 - 1) + distanceByIndex i2 (i2 + 1)
          newDistance = distanceByIndex (i1 - 1) i2 + distanceByIndex (i2 + 1) i1

opt2 :: [Edge] -> Solution
opt2 problem = solve ([0..length problem - 1], 0)
    where distances = distancesMap problem
          possibleSwaps' = possibleSwaps $ length problem
          bestImprovement (_, f1) (_, f2) = compare f1 f2
          possibleSolutions current = Prelude.map (neighbour distances current) possibleSwaps'
          nextSolution current = minimumBy bestImprovement $ possibleSolutions current
          solve (currentSolution, i)
            | i > 0 = currentSolution
            | otherwise = solve (nextSolution currentSolution)
