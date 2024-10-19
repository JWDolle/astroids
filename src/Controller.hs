{-# LANGUAGE RecordWildCards #-}

-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Entity
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random


-- | Handle one iteration of the game

-- GameState 
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState i e p) = return $ stepPure secs gstate
                      
stepPure :: Float -> GameState -> GameState
stepPure secs gstate@(GameState i e p@Player{..}) | (isMoving == True && isRotatingR == True && isRotatingL == True) = gstate { elapsedTime = elapsedTime gstate + secs, player = move p }
                                                  | (isMoving == True && isRotatingL == True) = gstate { elapsedTime = elapsedTime gstate + secs, player = rotate_ (move p) }
                                                  | (isMoving == True && isRotatingR == True) = gstate { elapsedTime = elapsedTime gstate + secs, player = rotate_ (move p) }
                                                 
                                                  | (isDecelling == True && isRotatingR == True) = gstate { elapsedTime = elapsedTime gstate + secs, player = rotate_ (move p)}
                                                  | isMoving     = gstate { elapsedTime = elapsedTime gstate + secs, player = move p }
                                                  | isDecelling = gstate { elapsedTime = elapsedTime gstate + secs, player = move p }
                                                  | (isRotatingL `xor` isRotatingR)    = gstate { elapsedTime = elapsedTime gstate + secs, player = rotate_ p }

                                                  | otherwise    = gstate { elapsedTime = elapsedTime gstate + secs, player = p}

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
inputKey _ gstate = gstate
-- Otherwise keep the same