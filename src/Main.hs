module Main where

import Tsp.Opt2
import Tsp.Common
import Data.Array.IO

main :: IO ()
main = do
        problem <- readFile "data/tsp_70_1"
        let edges = parse $ lines problem
        let solution = opt2 edges
        print solution
        print $ fitness (distancesMap edges) solution
-- main = do
--         arr <- newListArray (0, 4) [0..4]
--         arr1 <- swap' arr 1 3
--         l <- getElems arr1
--         print l

