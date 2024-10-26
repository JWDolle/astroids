-- | This module contains the data types
--   which represent the state of the game
module Model where
import Entity
import Player
import Projectile
import Sprites
import Enemy
import System.Random
import Constants
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
                 , comets :: [Comet]
                 , ufos :: [UFO]
                 , scatters :: [Scatter]
                 , lasers :: Lasers
                 , bullets :: Bullets
                 , random :: StdGen
                 , state :: State
                 --, sprites :: Sprites
                 }

initialState ::  GameState
initialState  = GameState ShowNothing 0 p1 [c1] [] [] [] [] seed Playing 