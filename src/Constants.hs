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
rAngle = 15

