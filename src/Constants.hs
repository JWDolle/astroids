-- All constants go here
module Constants where
import Graphics.Gloss

-- The number of frames for an animation to update
animationThreshold :: Int
animationThreshold = 5

-- The default picture in case of rendering errors
defaultPicture :: Picture
defaultPicture = blank


 --player Velocyt
pVelocity :: Float
pVelocity = 2;
-- player rotation angle
rAngle :: Float
rAngle = 7

type Lives = Int 


epsilon :: Float
epsilon = 1e-6  -- You can adjust this value as needed

-- Function to check if a float is approximately zero
isApproximatelyZero :: Float -> Bool
isApproximatelyZero x = abs x < epsilon