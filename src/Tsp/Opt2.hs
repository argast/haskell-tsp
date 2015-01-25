module Tsp.Opt2 where

import Tsp.Common
import Data.Array.IO
import Control.Monad
import Data.List
import qualified Data.Map.Strict as Map

possibleSwaps :: Int -> [(Int, Int)]
possibleSwaps size = [(i, j) | i <- [0..size - 1], j <- [i + 1..size - 1], i /= (j + 1) `mod` size, j - i < size - 2]

swap :: Solution -> (Int, Int) -> Solution
swap solution (i1, i2) = beginning ++ middle ++ end
    where beginning = take from solution
          middle = (reverse . take (to - from + 1) . drop from) solution
          end = drop (to + 1) solution
          from = min i1 i2
          to = max i1 i2

swap' :: IOArray Int Int -> Int -> Int -> IO (IOArray Int Int)
swap' arr i j = do
    forM_ [0..((j - i) `mod` 2)] $ \k -> do
        vi <- readArray arr (i + k)
        vj <- readArray arr (j - k)
        writeArray arr (i + k) vj
        writeArray arr (j - k) vi
    return arr

improvement :: Distances -> Solution -> (Int, Int) -> Double
improvement distances solution (i1, i2) = newDistance - oldDistance
    where size = length solution
          ind i = (i + size) `mod` size
          edgeByIndex i = solution !! ind i
          distanceByIndex ind1 ind2 = distances Map.! (edgeByIndex ind1, edgeByIndex ind2)
          oldDistance = distanceByIndex i1 (i1 - 1) + distanceByIndex i2 (i2 + 1)
          newDistance = distanceByIndex (i1 - 1) i2 + distanceByIndex (i2 + 1) i1

opt2 :: [Edge] -> Solution
opt2 problem = solve ([0..length problem - 1], 0)
    where bestImprovement (_, f1) (_, f2) = compare f1 f2
          distances = distancesMap problem
          possibleSwaps' = possibleSwaps $ length problem
          solve (currentSolution, lastImprovement)
            | lastImprovement > 0 = currentSolution
            | otherwise = solve (nextSolution)
                where nextSolution = let (bestSwap, difference) = minimumBy bestImprovement $ swaps
                                     in (swap currentSolution bestSwap, difference)
                      swaps = map (\s -> (s, improvement' s)) possibleSwaps'
                      improvement' = improvement distances currentSolution
