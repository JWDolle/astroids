{-# LANGUAGE RecordWildCards #-}


module Updates where
import Player
import Model
import Enemy
import Entity
import BoundingBox


-- Function to update player movement
updateMovement :: Player -> GameState -> GameState
updateMovement p gstate =
    let newPlayer = if isMoving p || isDecelling p
                    then move p
                    else p
    in gstate { player = newPlayer }

-- Function to update player rotation
updateRotation :: Player -> GameState -> GameState
updateRotation p gstate =
    let rotatedPlayer = if isRotatingL p || isRotatingR p
                        then rotate_ p
                        else p
    in gstate { player = rotatedPlayer }

updateEnemies :: [Enemy] -> GameState -> GameState
updateEnemies enemies gstate = 
        let newEnemies = map updateEnemy enemies
        in gstate {enemies = newEnemies}

updateEnemy :: Enemy -> Enemy
updateEnemy e@(C c)= rotate_ $ move e
updateEnemy e@(S s)= rotate_ $ move e
updateEnemy e@(U u) = move e

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
    }
 