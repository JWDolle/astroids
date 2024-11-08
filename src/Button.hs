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
playButton  = Button{ bShape = shape, bLocation = (0 - playWidth / 2,-100 - playHeigth / 2)}
    where 
        shape =  polygon[(0,0), (0,50), (100,50), (100,0)]

whithinButton:: Point -> Point ->  Float -> Float -> Bool
whithinButton (x,y) (bx, by) w h = x < (bx + w) && x > bx && y < (by + h) && y > by

exitButton:: Button
exitButton = Button {bShape = e, bLocation = (340, 330)}
    where
        e = polygon [(0,0),(32,0),(32,32),(0,32)]
