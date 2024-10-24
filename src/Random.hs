module Random where

import System.Random

-- Generates a random seed
randomSeed :: Int -> StdGen
randomSeed = mkStdGen

-- Returns a value from within a range
randomRange :: Random a => (a,a) -> StdGen -> (a,StdGen)
randomRange range gen = randomR range gen

