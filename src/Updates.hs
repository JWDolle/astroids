{-# LANGUAGE RecordWildCards #-}



module Updates where
import Player
import Model
import Enemy
import Entity
import BoundingBox
import Projectile
import Random
import Constants


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
updateEnemies gstate@GameState{..} = if length comets > 2 then gstate{comets = map (\x -> rotate_ (move x)) comets,
                                                  scatters = map (\x -> rotate_ (move x))scatters,
                                                  ufos = map (\x -> move x) ufos} else spawnEnemy c1 gstate

spawnEnemy :: Comet -> GameState -> GameState
spawnEnemy (Comet n liv loc dir f sh sp ro bb) gState@(GameState i e p c u s l b r Playing) = (GameState i e p (newCom:c) u s l b (snd randomDirectionY) Playing)
    where

        randomLocationX = randomRange (0,screenSize) r
        randomLocationY = randomRange (0,screenSize) (snd randomLocationX)

        randomDirectionX = randomRange (0,2) (snd randomLocationY)
        randomDirectionY = randomRange (0,2) (snd randomDirectionX)

        newCom = (Comet n liv ((fromIntegral (fst randomLocationX))- ((fromIntegral screenSize) / 2), (fromIntegral (fst randomLocationY)) - ((fromIntegral screenSize) / 2)) ((fst randomDirectionX) - 1, (fst randomDirectionY) - 1) f sh sp ro bb)


updatePlayer:: GameState -> GameState
updatePlayer gstate@(GameState _ _ p _ _ _ _ _ _ _)= 
    let
        movedPlayer = updateMovementPlayer p gstate
        rotatedPlayer = updateRotationPlayer (player movedPlayer) movedPlayer
        updatedPlayer = rotatedPlayer
    in  updatedPlayer

updateBullets:: Float -> GameState -> GameState
updateBullets secs gstate@(GameState _ _ _ _ _ _ _ b _ _) =
    let 
        timedOut = filterProjectiles (map (\x -> outOfTime x secs ) b)
        movedBullets = map move timedOut
        updatedBullets = gstate{ bullets = filterProjectiles movedBullets }
    in  updatedBullets






 