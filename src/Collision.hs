
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


----EVERYTHING THAT NEEDS TO HAPPEN WHEN THERE IS A COLLISION


      
    

handleCollision :: GameState -> GameState
handleCollision gstate@(GameState i e p c u s l b Playing ) =
    let playerCollision  = handlePlayerCollision p gstate
        newBullets       = map (\x -> handleBulletCollisions x gstate) b
        cometCollision   = map (\z -> handleCometCollision z gstate) c
        scatterCollision = map (\y -> handleScatterCollision y gstate) s
        ufoCollision     = map (\w -> handleUfoCollision w gstate) u
        newState | pLives playerCollision == 0 = GameOver
                 | otherwise = Playing
        filteredBullets  = filterProjectiles newBullets
        filteredComets   = filter (\a ->  cLives a > 0 ) cometCollision
        filteredScatter  = filter (\t ->  sLives t > 0 ) scatterCollision
        filteredUfo      = filter (\k ->  uLives k > 0 ) ufoCollision
    in gstate { player = playerCollision, comets = filteredComets, ufos = filteredUfo, scatters = filteredScatter, bullets = filteredBullets, state = newState} 


handlePlayerCollision :: Player -> GameState -> Player
handlePlayerCollision p gstate@GameState{..} |  checkCollision p comets || checkCollision p scatters || checkCollision p ufos =  p 
                        { pLives = pLives p - 1
                        , pLocation = pLocation p1  -- Replace this with actual logic for new location
                        , bb = bb p1
                        , pSpeed = 0
                        , pMovedir = pMovedir p1
                        } 
                                             | otherwise = player



checkCollision :: (HasBounding b, HasBounding a) => a -> [b] -> Bool
checkCollision a bs = 
    any (collide a ) bs



handleBulletCollisions :: Projectile -> GameState -> Projectile
handleBulletCollisions bullet gstate@GameState{..}  | checkCollision bullet comets || checkCollision bullet scatters || checkCollision bullet ufos = bullet{prAlive = False}
                                                    | otherwise = bullet

handleCometCollision :: Comet -> GameState -> Comet
handleCometCollision c gstate@GameState{..}     | checkCollision c bullets = c {cLives = cLives c - 1}
                                                | otherwise = c

handleScatterCollision :: Scatter -> GameState -> Scatter
handleScatterCollision s gstate@GameState{..}  | checkCollision s bullets = s {sLives = sLives s - 1}
                                               | otherwise = s


handleUfoCollision :: UFO -> GameState -> UFO
handleUfoCollision u gstate@GameState{..}    | checkCollision u bullets = u {uLives = uLives u - 1}
                                             | otherwise = u