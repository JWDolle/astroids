module Score where

import System.IO
import Control.Exception
import Data.List
import Constants

addHighScore :: Int -> IO ()
addHighScore score = do
    input <- readFromFile scoreFilePath
    let output = evaluateScores score input
    writeToFile scoreFilePath output

-- reads a file using a filepath and returns the contents of the file as a string
readFromFile :: FilePath -> IO String
readFromFile filePath = do
    withFile filePath ReadMode $ \ handle -> do
        content <- hGetContents handle
        evaluate (length content)
        return content

-- writes a string to a file at the filepath
writeToFile :: FilePath -> String -> IO ()
writeToFile filePath string = withFile filePath WriteMode $ \handle -> do
    hPutStr handle string

evaluateScores :: Int -> String -> String
evaluateScores score string = unlines $ map show (reverse $ tail $ sort $ score : (map read (lines string)))

stringToIntList :: String -> [Int]
stringToIntList s = map read (lines s)

addScoreToList :: Int -> [Int] -> [Int]
addScoreToList score scores = scores ++ [score]

removeWorstScore :: [Int] -> [Int]
removeWorstScore scores = reverse (tail (sort scores))

intListToString :: [Int] -> String
intListToString ints = unlines (map show ints)