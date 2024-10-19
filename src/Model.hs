-- | This module contains the data types
--   which represent the state of the game
module Model where
import Entity

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , player :: Player
                 }

initialState :: GameState
initialState = GameState ShowNothing 0 p1