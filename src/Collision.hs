
{-# LANGUAGE RecordWildCards #-}
module Collision where
import Graphics.Gloss
import BoundingBox
import Player
import Enemy
import Constants
import Entity
import Projectile
import Model 

data TypeCollision = PlayerEnemy | CometBullet | ScatterBullet | NoCollision deriving(Eq)

----EVERYTHING THAT NEEDS TO HAPPEN WHEN THERE IS A COLLISION
handleCollisions :: TypeCollision -> GameState -> GameState
handleCollisions t gstate@(GameState i e p c u s b l Playing) =
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
    , pMovedir = pMovedir p1
    }

handleBulletCollision :: Projectile -> Projectile
handleBulletCollision b = b{
    prAlive = False
}

handleLaserCollision :: Projectile -> Projectile
handleLaserCollision l = l{
    prAlive = False
}

handleCometCollision :: Comet -> Comet
handleCometCollision c = c {
    cLives = cLives c - 1
}
handleScatterCollision :: Scatter -> Scatter
handleScatterCollision s = s {
    sLives = sLives s - 1
}


checkCollision :: (HasBounding b, HasBounding a) => a -> [b] -> Bool
checkCollision a bs = 
    any (collide a ) bs

checkAllEnemyColision :: Player-> [Comet] -> [Scatter] -> [UFO] -> TypeCollision
checkAllEnemyColision player cs ss ufos | checkCollision player cs || checkCollision player ss || checkCollision player ufos = PlayerEnemy
                                        | otherwise = NoCollision

checkBulletCollisions :: Projectile -> [Comet] -> [Scatter] -> TypeCollision
checkBulletCollisions bullet cs ss     | checkCollision bullet cs = CometBullet
                                       | checkCollision bullet ss = ScatterBullet
                                       | otherwise = NoCollision

