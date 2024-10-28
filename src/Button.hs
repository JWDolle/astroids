{-# LANGUAGE RecordWildCards #-}

module Button where
import Graphics.Gloss


data Button = Button{bShape :: Picture, bLocation :: Point}

playWidth :: Float
playWidth = 100
playHeigth :: Float
playHeigth = 50

exitWidth :: Float
exitWidth = 32
exitHeigth :: Float
exitHeigth = 32

playButton :: Button
playButton  = Button{ bShape = shape, bLocation = (0,0)}
    where 
        shape =  polygon[(0,0), (0,50), (100,50), (100,0)]

whithinButton:: Point -> Point ->  Float -> Float -> Bool
whithinButton (x,y) (bx, by) w h = abs(x - bx) < w && abs(y - by) < h

exitButton:: Button
exitButton = Button {bShape = e, bLocation = (270, 340)}
    where
        e = color green $ polygon [(0,0),(32,0),(32,32),(0,32)]
             