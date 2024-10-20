{-# LANGUAGE RecordWildCards #-}

-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Constants
import Player
import Model
import Renderable
import Enemy
import BoundingBox
import Entity

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pictures [pictures (map render (  enemies gstate)), color red $ debugDirection (player gstate), color white . translate (-200) 0 .scale 0.1 0.1 $ debugPlayer gstate, render (player gstate), drawBoundingBox (bb (player gstate))  ]
























-------DEBUG----------------------
debugDirection :: Player -> Picture
debugDirection Player{..} =
    let (x, y) = pLocation         -- Get player location
        (dx, dy) = pMovedir      -- Get direction vector
        directionEnd = (x + dx * 50, y + dy * 50)  -- Calculate end point
    in line [ (x, y), directionEnd ]  -- Draw a line from the location to the end point
debugPlayer :: GameState -> Picture
debugPlayer gstate = pictures [
    translate (-180) 680  textLine1,
    translate (-180) 480  textLine2,
    translate (-180) 280  textLine3,
    translate (-180) 80   textLine4,
    translate (-180) (-120) textLine5,
    translate (-180) (-320) textLine6,
    translate (-180) (-520) textLine7,
    translate (-180) (-720) textLine8,
    translate (-180) (-920) textLine9,
    translate (-180) (-1120) textLine10,  -- First corner
    translate (-180) (-1320) textLine11,  -- Second corner
    translate (-180) (-1520) textLine12,  -- Third corner
    translate (-180) (-1720) textLine13   -- Fourth corner
    ]
  where
    cplayer = player gstate  -- Extract the player from the game state
    bounding = bb (player gstate)  -- Extract bounding box from the player
    corners = bbCorners bounding   -- Get the bounding box corners
    
    -- Text output for the player's information
    textLine1 = text $ "bbRotation: " ++ show (rotation bounding)
    textLine2 = text $ "Lives: " ++ show (pLives cplayer)
    textLine3 = text $ "Location: " ++ show (pLocation cplayer)
    textLine4 = text $ "Direction: " ++ show (extractAngle (pMovedir cplayer))
    textLine5 = text $ "Speed: " ++ show (pSpeed cplayer)
    textLine6 = text $ "Is Moving: " ++ show (isMoving cplayer)
    textLine7 = text $ "Speed: " ++ show (pSpeed cplayer)
    textLine8 = text $ "BoundingBox Center: " ++ show ((centerX bounding, centerY bounding))
    textLine9 = text $ "Is Decelling: " ++ show (isDecelling cplayer)

    -- Text output for the bounding box corners
    textLine10 = text $ "BB Corner 1: " ++ show (corners !! 0)  -- First corner
    textLine11 = text $ "BB Corner 2: " ++ show (corners !! 1)  -- Second corner
    textLine12 = text $ "BB Corner 3: " ++ show (corners !! 2)  -- Third corner
    textLine13 = text $ "BB Corner 4: " ++ show (corners !! 3)  -- Fourth corner


drawBoundingBox :: BoundingBox -> Picture
drawBoundingBox  bb =
    let corners = bbCorners bb  -- Get the corners of the bounding box
        centerPoint = (centerX bb, centerY bb) -- Get the center of the bounding box
        cornerLines = rotate (radians (rotation bb)) $ lineLoop corners -- Create a polygon that connects the corners
        centerDot = translate 0 0 $ color red $ circleSolid 5
    in pictures [color green cornerLines, centerDot] -- 