-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Entity
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate
  = -- We show a new random number
    do      
       return $ gstate { elapsedTime = elapsedTime gstate + secs }


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (char 'w') Down _ _ _) = 
inputKey _ gstate = gstate -- Otherwise keep the same