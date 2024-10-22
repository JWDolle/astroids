

module Collision where
import Graphics.Gloss
import BoundingBox
import Player
import Enemy
import Constants
import Entity
import Model 



----EVERYTHING THAT NEEDS TO HAPPEN WHEN THERE IS A COLLISION
handleCollision :: GameState -> GameState
handleCollision gstate@(GameState i e p c u s Playing) =
    let updatedPlayer = handlePlayerCollision p
        newState | pLives updatedPlayer == 0 = GameOver
                 | otherwise = Playing
    in gstate { player = updatedPlayer, state = newState}

handlePlayerCollision :: Player -> Player
handlePlayerCollision p = p 
    { pLives = pLives p - 1
    , pLocation = pLocation p1  -- Replace this with actual logic for new location
    , bb = bb p1
    , pSpeed = 0
    }

checkCollision :: (HasBounding b, HasBounding a) => a -> [b] -> Bool
checkCollision a bs = 
    any (collide a ) bs

checkAllEnemyColision :: Player-> [Comet] -> [Scatter] -> [UFO] -> Bool
checkAllEnemyColision player cs ss ufos = checkCollision player cs || checkCollision player ss || checkCollision player ufos
