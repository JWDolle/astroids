-- All constants go here
module Constants where
import Graphics.Gloss
import Random
import System.Random

--
screenSize :: Int
screenSize = 800

-- The number of frames for an animation to update
animationThreshold :: Int
animationThreshold = 5

-- The default picture in case of rendering errors
defaultPicture :: Picture
defaultPicture = blank

c_rAngle :: Float
c_rAngle = 2

 --player Velocyt
pVelocity :: Float
pVelocity = 2;
-- player rotation angle
rAngle :: Float
rAngle = 7

-- The starting random seed
seed :: StdGen
seed = randomSeed 50

-- Filepath to the highscores file
scoreFilePath :: FilePath
scoreFilePath = "src/highscores.txt"

-- Radius at which enemies can spawn from the player
spawningRadius :: Float
spawningRadius = 100

bulletSpeed :: Float
bulletSpeed = 4

epsilon :: Float
epsilon = 1e-6  -- You can adjust this value as needed



frameRate :: Int
frameRate = 60

bulletExistance :: Int 
bulletExistance = secToframes 2 -- meaning it will exist for 2 seconds

shootCooldown:: Int
shootCooldown = 5

accel:: Float
accel = 0.1

decel :: Float
decel = 0.1



radians:: Float -> Float
radians d = d * (pi/180)

degrees :: Float -> Float
degrees r =  r * (180/pi)

extractAngle :: Vector -> Float
extractAngle v = atan2 (snd v) (fst v)

-- Function to check if a float is approximately zero
isApproximatelyZero :: Float -> Bool
isApproximatelyZero x = abs x < epsilon

normalize :: (Float, Float) -> (Float, Float)
normalize (x, y) = 
    let len = sqrt (x^2 + y^2)
    in if len == 0 then (0, 0) else (x / len, y / len)

rotateVector :: (Float, Float) -> Float -> (Float, Float)
rotateVector (x, y) angleDeg =
    let angleRad = radians (degrees angleDeg)
        cosA = cos angleRad
        sinA = sin angleRad
    in (x * cosA - y * sinA, x * sinA + y * cosA)

secToframes:: Int -> Int
secToframes secs = secs * frameRate