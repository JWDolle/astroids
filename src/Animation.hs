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
} deriving (Renderable)

instance Renderable Animation where
    render = renderAnimation

-- Renders the current frame of an animation
renderAnimation :: Animation -> Picture
renderAnimation (Animation counter frameIndex frames) | frameIndex >= 0 && frameIndex < length frames = frames !! frameIndex
                                                      | otherwise = defaultPicture

-- Updates an animation when the counter exceeds the animation threshold                                 
updateAnimation :: Animation -> Animation
updateAnimation (Animation counter frameIndex frames) | counter > animationThreshold && frameIndex + 1 < length frames = Animation 0 (frameIndex + 1) frames 
                                                      | counter > animationThreshold = Animation 0 0 frames
                                                      | otherwise = Animation (counter + 1) frameIndex frames
