-- We also will have a data type for animations that derives from the Renderable type class
--	data Animation = Animate { 
--				counter :: Int
--				frameIndex :: Int
--				frames :: [Picture]
--				} deriving (Renderable)

module Animation where
import Animation
import Renderable
import Constants

data Animation = Animate {
    counter :: Int
    frameIndex :: Int
    frames :: [Picture]
    repeating :: Bool
} deriving (Renderable)

instance Renderable Animation where
    render = renderAnimation

-- Renders the current frame of an animation
renderAnimation :: Animation -> Picture
renderAnimation (Animation _ frameIndex frames _) | frameIndex >= 0 && frameIndex < length frames = frames !! frameIndex
                                                  | otherwise = defaultPicture

-- Updates an animation when the counter exceeds the animation threshold                                 
updateAnimation :: Animation -> Animation
updateAnimation (Animation counter frameIndex frames repeating) | counter > animationThreshold && not repeating = Animation 0 (frameIndex + 1) frames repeating
                                                                | counter > animationThreshold && frameIndex + 1 < length frames = Animation 0 (frameIndex + 1) frames repeating 
                                                                | counter > animationThreshold = Animation 0 0 frames repeating
                                                                | otherwise = Animation (counter + 1) frameIndex frames repeating
