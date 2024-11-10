module Score where

import System.IO
import Control.Exception
import Data.List
import Constants

-- Adds a highscore to the highscores file
addHighScore :: Int -> IO ()
addHighScore score = do
    input <- readFromFile scoreFilePath
    let output = evaluateScores score input
    writeToFile scoreFilePath output
    where
        -- writes a string to a file at the filepath
        writeToFile :: FilePath -> String -> IO ()
        writeToFile filePath string = withFile filePath WriteMode $ \handle -> do
            hPutStr handle string

        -- unpacks the file data, adds the score and reverts it back to a string
        evaluateScores :: Int -> String -> String
        evaluateScores score string = unlines $ map show $ reverse $ tail $ sort $ score : (getScores string)

-- reads a file using a filepath and returns the contents of the file as a string
readFromFile :: FilePath -> IO String
readFromFile filePath = do
    withFile filePath ReadMode $ \ handle -> do
        content <- hGetContents handle
        evaluate (length content)
        return content

-- Obtains the scores from a string
getScores :: String -> [Int]
getScores string = map read $ lines string