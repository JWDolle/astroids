{-# LANGUAGE RecordWildCards #-}

-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Entity
import Model
import Renderable


view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [color red $ debugDirection (player gstate), color white . translate (-200) 0 .scale 0.1 0.1 $ debugPlayer gstate, render (player gstate) ]

debugDirection :: Player -> Picture
debugDirection Player{..} =
    let (x, y) = pLocation         -- Get player location
        (dx, dy) = pMovedir      -- Get direction vector
        directionEnd = (x + dx * 50, y + dy * 50)  -- Calculate end point
    in line [ (x, y), directionEnd ]  -- Draw a line from the location to the end point

debugPlayer :: GameState -> Picture
debugPlayer gstate = pictures [translate (-180) 680  textLine1,
                                translate (-180) 520 textLine2,
                                translate (-180) 360 textLine3,
                                translate (-180) 200 textLine4,
                                translate (-180) 40 textLine5,
                                translate (-180) (-120) textLine6,
                                translate (-180) (-280) textLine7]
  where
    cplayer = player gstate  -- Extract the player from the game state
    textLine1 = text $ "Name: " ++ pName cplayer
    textLine2 = text $ "Lives: " ++ show (pLives cplayer)
    textLine3 = text $ "Location: " ++ show (pLocation cplayer)
    textLine4 = text $ "Direction: " ++ show (pMovedir cplayer)
    textLine5 = text $ "Speed: " ++ show (pSpeed cplayer)
    textLine6 = text $ "Is Moving: " ++ show (isMoving cplayer)
    textLine7 = text $ "Speed " ++ show (pSpeed cplayer)
