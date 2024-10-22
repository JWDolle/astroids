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

updateEnemies ::  GameState -> GameState
updateEnemies gstate@(GameState _ _ _ c u s _ ) = gstate{comets = map (\x -> rotate_ (move x)) c,
                                                  scatters = map (\x -> rotate_ (move x))s,
                                                  ufos = map (\x -> move x) u}


updatePlayer:: GameState -> GameState
updatePlayer gstate@(GameState _ _ p _ _ _ _)= 
    let
        movedPlayer = updateMovement p gstate
        rotatedPlayer = updateRotation (player movedPlayer) movedPlayer
        updatedPlayer = rotatedPlayer
    in  updatedPlayer







 