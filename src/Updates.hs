module Updates where
import Player
import Model
import Enemy
import Entity


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
