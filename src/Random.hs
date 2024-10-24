module Random where

import System.Random

randomSeed :: Int -> StdGen
randomSeed = mkStdGen

randomRange :: RandomGen a => (a,a) -> StdGen -> (a,StdGen)
randomRange range gen = randomR range gen

