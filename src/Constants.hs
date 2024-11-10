-- All constants go here
module Constants where
import Graphics.Gloss
import Random
import System.Random
import Button

-- Bool to whether to display the debug information
displayDebug :: Bool
displayDebug = False

-- Size of the Screen
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
bulletSpeed = 6

epsilon :: Float
epsilon = 1e-6  -- You can adjust this value as needed


-- Framerate of the game
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

-- Score gained from killing a comet
cometScore :: Int
cometScore = 25

-- Score gained from killing a comet
scatterScore :: Int
scatterScore = 100

-- Score gained from killing a comet
ufoScore :: Int
ufoScore = 200

ufoWidth:: Float
ufoWidth = 60

ufoHeigth:: Float
ufoHeigth = 60

laserCooldown:: Int
laserCooldown  = secToframes 2

playWidth :: Float
playWidth = 100

playHeigth :: Float
playHeigth = 50

exitWidth :: Float
exitWidth = 32

exitHeigth :: Float
exitHeigth = 32

-- Creates a play button
playButton :: Button
playButton  = Button{ bShape = shape, bLocation = (0 - playWidth / 2,-100 - playHeigth / 2)}
    where 
        shape =  polygon[(0,0), (0,50), (100,50), (100,0)]

-- Creates an exit button
exitButton :: Button
exitButton = Button {bShape = e, bLocation = (340, 330)}
    where
        e = polygon [(0,0),(32,0),(32,32),(0,32)]

cometWidth :: Float
cometWidth = 30

cometHeigth :: Float
cometHeigth = 30

scatterWidth :: Float
scatterWidth = 60

scatterHeigth:: Float
scatterHeigth = 60

-- Stores the animation frames for when the player moves
playerAnimationFrames :: [Picture]
playerAnimationFrames = [polygon[(0,0), (30,15), (0,30)], pictures [polygon[(0,0), (30,15), (0,30)], polygon[(-5,8), (-15,15), (-5,22)]], pictures [polygon[(0,0), (30,15), (0,30)], polygon[(-5,8), (-20,15), (-5,22)]], pictures [polygon[(0,0), (30,15), (0,30)], polygon[(-5,8), (-15,15), (-5,22)]]]

maxSpeed :: Float
maxSpeed = 5

player1Local:: Point
player1Local = (0,0)

playerWidth :: Float
playerWidth = 30

playerHeigth :: Float
playerHeigth= 30

projectileWidth :: Float
projectileWidth = 10
