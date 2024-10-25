{-# LANGUAGE RecordWildCards #-}



module Updates where
import Player
import Model
import Enemy
import Entity
import BoundingBox
import Projectile


-- Function to update player movement
updateMovementPlayer :: Player -> GameState -> GameState
updateMovementPlayer p gstate =
    let newPlayer = if isMoving p || isDecelling p
                    then move p
                    else p
    in gstate { player = newPlayer }

-- Function to update player rotation
updateRotationPlayer :: Player -> GameState -> GameState
updateRotationPlayer p gstate =
    let rotatedPlayer = if isRotatingL p || isRotatingR p
                        then rotate_ p
                        else p
    in gstate { player = rotatedPlayer }

updateEnemies ::  GameState -> GameState
updateEnemies gstate@GameState{..} = gstate{comets = map (\x -> rotate_ (move x)) comets,
                                                  scatters = map (\x -> rotate_ (move x))scatters,
                                                  ufos = map (\x -> move x) ufos}


updatePlayer:: GameState -> GameState
updatePlayer gstate@(GameState _ _ p _ _ _ _ _ _)= 
    let
        movedPlayer = updateMovementPlayer p gstate
        rotatedPlayer = updateRotationPlayer (player movedPlayer) movedPlayer
        updatedPlayer = rotatedPlayer
    in  updatedPlayer

updateBullets:: GameState -> GameState
updateBullets gstate@(GameState _ _ _ _ _ _ _ b _ ) =
    let 
        movedBullets = map move b
        updatedBullets = gstate{ bullets = filterProjectiles movedBullets }
    in  updatedBullets






 