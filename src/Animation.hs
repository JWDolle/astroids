module Animation where
import Constants
import Graphics.Gloss

-- Datatype for animations
data Animation = Animate {
    counter :: Int,
    frameIndex :: Int,
    frames :: [Picture],
    repeating :: Bool
} 

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
