-- All constants go here
module Constants where
import Graphics.Gloss
import Random
import System.Random

--
screenSize :: Int
screenSize = 400

-- The number of frames for an animation to update
animationThreshold :: Int
animationThreshold = 5

-- The default picture in case of rendering errors
defaultPicture :: Picture
defaultPicture = blank

c_rAngle :: Float
c_rAngle = 0.1

 --player Velocyt
pVelocity :: Float
pVelocity = 2;
-- player rotation angle
rAngle :: Float
rAngle = 7

-- The starting random seed
seed :: StdGen
seed = randomSeed 1

-- Filepath to the highscores file
scoreFilePath :: FilePath
scoreFilePath = "src/highscores.txt"


bulletSpeed :: Float
bulletSpeed = 4

epsilon :: Float
epsilon = 1e-6  -- You can adjust this value as needed

-- Function to check if a float is approximately zero
isApproximatelyZero :: Float -> Bool
isApproximatelyZero x = abs x < epsilon

normalize :: (Float, Float) -> (Float, Float)
normalize (x, y) = 
    let len = sqrt (x^2 + y^2)
    in if len == 0 then (0, 0) else (x / len, y / len)