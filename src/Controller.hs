{-# LANGUAGE RecordWildCards #-}

-- | This module defines how the state changes
--   in response to time and user input
module Controller where
import Pipeline
import Model
import Player
import Projectile
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


-- | Handle one iteration of the game

-- GameState 
step :: Float -> GameState -> IO GameState
step secs gstate@GameState{..} = return $ stepPure secs gstate
  
stepPure :: Float -> GameState -> GameState
stepPure secs gstate@GameState{state = Playing} = pipeline1 secs gstate


stepPure sec gstate@GameState{state = Paused} = gstate
stepPure sec gstate@GameState{state = GameOver} = gstate
    
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)
                      

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'w') Down _ _ ) gstate = gstate {player = ((player gstate) {isMoving = True})} -- if w down we move
inputKey (EventKey (Char 'w') Up _ _ ) gstate = gstate {player = ((player gstate) {isMoving = False, isDecelling = True})} -- if w up we stop moving
inputKey (EventKey (Char 'd') Down _ _ ) gstate = gstate {player = ((player gstate) {isRotatingL = True, 
                                                                                    isRotatingR = False})} 
inputKey (EventKey (Char 'd') Up _ _ ) gstate = gstate {player = ((player gstate) {isRotatingL = False})}
inputKey (EventKey (Char 'a') Down _ _ ) gstate = gstate {player = ((player gstate) {isRotatingR = True, 
                                                                                    isRotatingL = False})} 
inputKey (EventKey (Char 'a') Up _ _ ) gstate = gstate {player = ((player gstate) {isRotatingR = False})}


inputKey (EventKey (Char 'p') Down _ _) gstate@GameState{state = Playing}= gstate {state = Paused}
inputKey (EventKey (Char 'p') Down _ _) gstate@GameState{state = Paused}= gstate {state = Playing}

inputKey (EventKey (SpecialKey KeySpace)Up _ _)  gstate@GameState{..} = gstate {bullets = spawnBullet (createbullet player) bullets }


inputKey _ gstate = gstate
-- Otherwise keep the same


