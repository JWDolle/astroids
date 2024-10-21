-- | This module contains the data types
--   which represent the state of the game
module Model where
import Entity
import Player
import Enemy
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
data State = Playing | Paused | GameOver

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

data GameState = GameState {
                   infoToShow  :: InfoToShow
                 , elapsedTime :: Float
                 , player :: Player
                 , enemies  :: [Enemy]
                 , state :: State
                 }

initialState :: GameState
initialState = GameState ShowNothing 0 p1 [C c1] Playing