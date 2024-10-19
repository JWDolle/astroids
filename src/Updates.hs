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