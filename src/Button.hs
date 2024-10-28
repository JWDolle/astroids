{-# LANGUAGE RecordWildCards #-}

module Button where
import Graphics.Gloss


data Button = Button{bShape :: Picture, bLocation :: Point}

playWidth :: Float
playWidth = 100
playHeigth :: Float
playHeigth = 50


playButton :: Button
playButton  = Button{ bShape = shape, bLocation = (0,0)}
    where 
        shape =  polygon[(0,0), (0,50), (100,50), (100,0)]

whithinButton:: Point -> Point ->  Float -> Float -> Bool
whithinButton (x,y) (bx, by) w h = abs(x - bx) < w && abs(y - by) < h