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
import Projectile
import Button
import Entity
import Score

view :: GameState -> IO Picture
view gstate@(GameState _ _ _ _ _ _ _ _ _ _ GameOver) = do 
    scores <- drawHighScores
    return $ pictures [scores, viewPure gstate]
view gstate = return $ viewPure gstate

viewPure :: GameState -> Picture
viewPure gstate@GameState{state = Playing} = pictures [pictures (map render (comets gstate)), 
                            
                                            color red $ debugDirection (player gstate), 
                                            
                                            color white . translate (-200) 0 .scale 0.1 0.1 $ debugPlayer gstate,
                                            
                                            render (player gstate), 
                                            renderBullets (bullets gstate),
                                            drawBoundingBox (bb (player gstate)), pictures (map render(scatters gstate)), 
                                            
                                            pictures (map drawBoundingBox ( map getBB (comets gstate))) ,pictures (map drawBoundingBox ( map getBB (bullets gstate))), render exitButton]
viewPure gstate@GameState{state = Menu}   = pictures [ color red $ render playButton]
viewPure gstate@GameState{state = Paused} = pictures [color white $ translate (-250) (100) $ text "PAUSED"]
viewPure gstate@GameState{state = GameOver} = Blank


drawHighScores :: IO Picture
drawHighScores = do
    scoreString <- readFromFile scoreFilePath
    let scores = getScores scoreString
    let pic = pictures [
            color white $ scale 0.1 0.1 $ translate (400) 680  (text $ "Highscores:"),
            color white $ scale 0.1 0.1 $ translate (400) 480  (text $ "1: " ++ show (scores !! 0)),
            color white $ scale 0.1 0.1 $ translate (400) 280  (text $ "2: " ++ show (scores !! 1)),
            color white $ scale 0.1 0.1 $ translate (400) 80   (text $ "3: " ++ show (scores !! 2)),
            color white $ scale 0.1 0.1 $ translate (400) (-120) (text $ "4: " ++ show (scores !! 3)),
            color white $ scale 0.1 0.1 $ translate (400) (-320) (text $ "5: " ++ show (scores !! 4))]
    return pic





















-------DEBUG----------------------
debugDirection :: Player -> Picture
debugDirection Player{..} =
    let (x, y) = pLocation         -- Get player location
        (dx, dy) | isDecelling = pFacing
                 | otherwise = pMovedir-- Get direction vector
        directionEnd = (x + dx * 50, y + dy * 50)  -- Calculate end point
    in translate (playerWidth/2) (playerWidth/2) $ line [ (x, y), directionEnd ]  -- Draw a line from the location to the end point
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
    translate (-180) (-1720) textLine14,   -- Fourth corner
    translate (-180) (-1920) textLine15,
    translate (-180) (-2120) textLine16
    ]
  where
    cplayer = player gstate  -- Extract the player from the game state
    bounding = bb (player gstate)  -- Extract bounding box from the player
    corners = bbCorners bounding   -- Get the bounding box corners
    
    -- Text output for the player's information
    textLine1 = text $ "bbRotation: " ++ show (rotation bounding)
    textLine2 = text $ "Lives: " ++ show (pLives cplayer)
    textLine3 = text $ "Location: " ++ show (pLocation cplayer)
    textLine4 = text $ "Direction: " ++ show (degrees (extractAngle (pMovedir cplayer)))
    textLine5 = text $ "Speed: " ++ show (pSpeed cplayer)
    textLine6 = text $ "Is Moving: " ++ show (isMoving cplayer)
    textLine7 = text $ "Speed: " ++ show (pSpeed cplayer)
    textLine8 = text $ "BoundingBox Center: " ++ show (centerX bounding, centerY bounding)
    textLine9 = text $ "Is Decelling: " ++ show (isDecelling cplayer)

    -- Text output for the bounding box corners
    textLine10 = text $ "BB Corner 1: " ++ show (corners !! 0)  -- First corner
    textLine11 = text $ "BB Corner 2: " ++ show (corners !! 1)  -- Second corner
    textLine12 = text $ "BB Corner 3: " ++ show (corners !! 2)  -- Third corner
    textLine13 = text $ "BB Corner 4: " ++ show (corners !! 3)  -- Fourth corner

    -- Handle the bullet's prAlive field
    textLine14 = if not (null (bullets gstate))
                 then text $ "prAlive 1: " ++ show (length (bullets gstate))
                 else text $ "prAlive 1: "++ show (length (bullets gstate))
    
    textLine15 = text $ "Number of Enemies: " ++ show (length (comets gstate))
    textLine16 = text $ "Score: " ++ show (score gstate)


drawBoundingBox :: BoundingBox -> Picture
drawBoundingBox bb =
    let corners = bbCorners bb  -- Get the corners of the bounding box
        cx = centerX bb         -- Get the center X of the bounding box
        cy = centerY bb         -- Get the center Y of the bounding box
        
        -- First translate to the origin, then rotate, then translate back
        cornerLines = translate cx cy 
                      $ rotate (radians (rotation bb)) 
                      $ translate (-cx) (-cy) 
                      $ lineLoop corners  -- Create a polygon that connects the corners
                      
        -- Draw a red dot at the center of the bounding box
        centerDot = translate cx cy $ color red $ circleSolid 5

    in pictures [color green cornerLines, centerDot] 


