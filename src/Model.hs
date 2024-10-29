-- | This module contains the data types
--   which represent the state of the game
module Model where
import Entity
import Player
import Projectile
import Sprites
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
                 --, sprites :: Sprites
                 }

initialState ::  GameState
initialState  = GameState playButton 0 p1 [] [] [scat] [] [] seed 0 Menu 