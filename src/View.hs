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
import Graphics.Gloss (orange)
import Constants (defaultPicture)

-- renders the game
view :: GameState -> IO Picture
view gstate@(GameState _ _ _ _ _ _ _ _ _ _ GameOver) = do 
    scores <- drawHighScores
    return $ pictures [scores, viewPure gstate]
view gstate = return $ viewPure gstate

-- randers the game in a pure form
viewPure :: GameState -> Picture
viewPure gstate@GameState{state = Playing} = pictures [pictures (map render (comets gstate)),                                             
                                            drawDebug gstate, 
                                            drawUI gstate,
                                            render (player gstate), 
                                            renderBullets (bullets gstate),
                                            renderLasers (lasers gstate),                                            
                                            pictures (map render(scatters gstate)),
                                            pictures (map render(ufos gstate))]
viewPure gstate@GameState{state = Menu}   = pictures [color blue $ render playButton, 
                                                      color white $ translate (-335) (200) $ text "ASTEROIDS", 
                                                      color white $ scale 0.25 0.25 $ translate (-450) (-175)  $ text "Press to play:"]
viewPure gstate@GameState{state = Paused} = pictures [color white $ translate (-250) (100) $ text "PAUSED",
                                                      color white $ scale 0.25 0.25 $ translate (-850) (-175)  $ text "Press 'P' to resume game"]
viewPure gstate@GameState{state = GameOver} = pictures [color blue $ render playButton,
                                                        color white $ scale 0.25 0.25 $ translate (-550) (-175)  $ text "Return to menu:",
                                                        color white $ translate (-335) (200) $ text "GAMEOVER"]

-- Draws the highscores to the screen
drawHighScores :: IO Picture
drawHighScores = do
    scoreString <- readFromFile scoreFilePath
    let scores = getScores scoreString
    let pic = pictures [
            color white $ scale 0.2 0.2 $ translate (1000) 340  (text $ "Highscores:"),
            color white $ scale 0.2 0.2 $ translate (1000) 190  (text $ "1: " ++ show (scores !! 0)),
            color white $ scale 0.2 0.2 $ translate (1000) 40  (text $ "2: " ++ show (scores !! 1)),
            color white $ scale 0.2 0.2 $ translate (1000) (-110)   (text $ "3: " ++ show (scores !! 2)),
            color white $ scale 0.2 0.2 $ translate (1000) (-260) (text $ "4: " ++ show (scores !! 3)),
            color white $ scale 0.2 0.2 $ translate (1000) (-410) (text $ "5: " ++ show (scores !! 4))]
    return pic

-- Draws UI elements to the screen
drawUI :: GameState -> Picture
drawUI gstate@(GameState _ _ p _ _ _ _ _ _ s _) = pictures [color white $ scale 0.1 0.1 $ translate 3450 3700 $ text "Exit:",
                                                            color orange $ render exitButton,
                                                            color white $ scale 0.1 0.1 $ translate (-3800) 3800 $ text ("Lives: " ++ show (pLives p)),
                                                            color white $ scale 0.1 0.1 $ translate (-3800) 3600 $ text ("Score: " ++ show s)]


















-------DEBUG----------------------
-- Draws the debug
drawDebug :: GameState -> Picture
drawDebug gstate | displayDebug = pictures [color red $ debugDirection (player gstate),
                                            color white . translate (-200) 0 .scale 0.1 0.1 $ debugPlayer gstate,
                                            drawBoundingBox (bb (player gstate)), 
                                            pictures (map drawBoundingBox ( map getBB (comets gstate))) ,
                                            pictures (map drawBoundingBox ( map getBB (bullets gstate)))]
                 | otherwise = defaultPicture
-- Gets the direction of the player and displays it
debugDirection :: Player -> Picture
debugDirection Player{..} =
    let (x, y) = pLocation         -- Get player location
        (dx, dy) = pFacing-- Get direction vector
        directionEnd = (x + dx * 50, y + dy * 50)  -- Calculate end point
    in translate (playerWidth/2) (playerWidth/2) $ line [ (x, y), directionEnd ]  -- Draw a line from the location to the end point
-- Draws debug text
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
    translate (-180) (-2120) textLine16,
    translate (-180) (-2320) textLine17,
    translate (-180) (-2520) textLine18,
    translate (-180) (-2720) textLine19
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
    
    textLine15 = text $ "Number of Enemies: " ++ show (length (comets gstate) + length (scatters gstate) + length (ufos gstate))
    textLine16 = text $ "Score: " ++ show (score gstate)
    textLine17 = text $ "Lasers: " ++ show (length (lasers gstate))
    textLine18 = text $ "UFO dir: " ++ 
     if not (null (ufos gstate)) 
        then show (aimDir (head (ufos gstate))) 
        else "No UFOs"
    textLine19 = text $ "Time: " ++ show (elapsedTime gstate)
   

-- Draws a boundingbox around entities
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


