{-# LANGUAGE RecordWildCards #-}

-- | This module defines how the state changes
--   in response to time and user input
module Controller where
import Collision
import Model
import Entity
import Player
import Updates
import Enemy
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


-- | Handle one iteration of the game

-- GameState 
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState i e p c _) = return $ stepPure secs gstate
  
stepPure :: Float -> GameState -> GameState
stepPure secs gstate@(GameState i e p c Playing) = 
  --- THIS FUNCTIONS AS A PIPELINE THIS WILL RETURN A FULLY UPDATED GAME STATE
    let updatedState = gstate { elapsedTime = elapsedTime gstate + secs }
        playermoved   = updateMovement p updatedState  -- Update movement based on state
        playerotated = updateRotation (player playermoved) playermoved  -- Update rotation based on state
        enemiesUpdated  = updateEnemies (enemies playerotated) playerotated

        collisions = checkEnemyCollision (bb (player enemiesUpdated)) ( enemies enemiesUpdated)
        collisionChecked = if collisions  
                           then handleCollision enemiesUpdated
                           else enemiesUpdated
    in  collisionChecked

stepPure sec gstate@(GameState _ _ _ _ Paused) = gstate
stepPure sec gstate@(GameState _ _ _ _ GameOver) = gstate
    
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


inputKey (EventKey (Char 'p') Down _ _) gstate@(GameState _ _ _ _ Playing )= gstate {state = Paused}
inputKey (EventKey (Char 'p') Down _ _) gstate@(GameState _ _ _ _ Paused )= gstate {state = Playing}


inputKey _ gstate = gstate
-- Otherwise keep the same


