

module Collision where
import Graphics.Gloss
import BoundingBox
import Player
import Enemy
import Constants
import Entity
import Model 

checkEnemyCollision :: BoundingBox -> [Enemy] -> Bool
checkEnemyCollision playerBB enemies = 
    any (collide playerBB . getEnemyBB) enemies

----EVERYTHING THAT NEEDS TO HAPPEN WHEN THERE IS A COLLISION
handleCollision :: GameState -> GameState
handleCollision gstate@(GameState i e p c Playing) =
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
 