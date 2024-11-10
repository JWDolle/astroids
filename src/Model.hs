-- | This module contains the data types
--   which represent the state of the game
module Model where
import Entity
import Player
import Projectile
import Enemy
import Button
import System.Random
import Constants
data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char
data State = Playing | Paused | GameOver | Menu

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 1

-- The gamestate for the game
data GameState = GameState {
                   button  :: Button
                 , elapsedTime :: Float
                 , player :: Player
                 , comets :: [Comet]
                 , ufos :: [UFO]
                 , scatters :: [Scatter]
                 , lasers :: Lasers
                 , bullets :: Bullets
                 , random :: StdGen
                 , score :: Int
                 , state :: State
                 }

-- The initial game state
initialState ::  GameState
initialState  = GameState playButton 0 p1 [] [] [] [] [] seed 0 Menu 