-- We also will have a data type for animations that derives from the Renderable type class
--	data Animation = Animate { 
--				counter :: Int
--				frameIndex :: Int
--				frames :: [Picture]
--				} deriving (Renderable)

module Animation where
--import Animation
--import Renderable
import Constants
import Graphics.Gloss

data Animation = Animate {
    counter :: Int,
    frameIndex :: Int,
    frames :: [Picture],
    repeating :: Bool
} 

-- instance Renderable Animation where
--     render = renderAnimation

-- Renders the current frame of an animation
renderAnimation :: Animation -> Picture
renderAnimation (Animate _ frameIndex frames _) | frameIndex >= 0 && frameIndex < length frames = frames !! frameIndex
                                                | otherwise = defaultPicture

-- Updates an animation when the counter exceeds the animation threshold                                 
updateAnimation :: Animation -> Animation
updateAnimation (Animate counter frameIndex frames repeating) | counter > animationThreshold && not repeating = (Animate 0 (frameIndex + 1) frames repeating)
                                                                | counter > animationThreshold && frameIndex + 1 < length frames = (Animate 0 (frameIndex + 1) frames repeating)
                                                                | counter > animationThreshold = (Animate 0 0 frames repeating)
                                                                | otherwise = (Animate (counter + 1) frameIndex frames repeating)
