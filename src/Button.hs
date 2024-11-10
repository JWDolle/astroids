{-# LANGUAGE RecordWildCards #-}

module Button where
import Graphics.Gloss

-- Datatype for the button
data Button = Button{bShape :: Picture, bLocation :: Point}

-- Checks if a point is within a button
whithinButton:: Point -> Point ->  Float -> Float -> Bool
whithinButton (x,y) (bx, by) w h = x < (bx + w) && x > bx && y < (by + h) && y > by


