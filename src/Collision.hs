
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
handleCollision gstate@(GameState i e p c u s l b r Playing ) =
    let playerCollision  = handlePlayerCollision p gstate
        newBullets       = map (\x -> handleBulletCollisions x gstate) b
        cometCollision   = map (\z -> handleCometCollision z gstate) c
        scatterCollision = map (\y -> handleScatterCollision y gstate) s       
        ufoCollision     = map (\w -> handleUfoCollision w gstate) u

        deadScatters       = filter (\t -> sLives t == 0) scatterCollision
        newComets          = concatMap (`deadScatter` c) deadScatters
        newState | pLives playerCollision == 0 = GameOver
                 | otherwise = Playing

        
        filteredBullets  = filterProjectiles newBullets
        filteredComets   = filter (\a ->  cLives a > 0 ) cometCollision ++ newComets
        filteredScatter  = filter (\t ->  sLives t > 0 ) scatterCollision
        filteredUfo      = filter (\k ->  uLives k > 0 ) ufoCollision
    in gstate { player = playerCollision, comets = filteredComets, ufos = filteredUfo, scatters = filteredScatter, bullets = filteredBullets, random = r, state = newState} 



handlePlayerCollision :: Player -> GameState -> Player
handlePlayerCollision p gstate@GameState{..} |  checkCollision p comets || checkCollision p scatters || checkCollision p ufos =  p 
                        { pLives = pLives p - 1
                        , pLocation = pLocation p1  -- Replace this with actual logic for new location
                        , bb = bb p1
                        , pSpeed = 0
                        , pMovedir = pMovedir p1
                        , pFacing = pFacing p1
                        } 
                                             | otherwise = player



checkCollision :: (HasBounding b, HasBounding a) => a -> [b] -> Bool
checkCollision a bs = 
    any (collide a ) bs



handleBulletCollisions :: Projectile -> GameState -> Projectile
handleBulletCollisions bullet gstate@GameState{..}  | checkCollision bullet comets || checkCollision bullet scatters || checkCollision bullet ufos = bullet{prAlive = False}
                                                    | otherwise = bullet

handleCometCollision :: Comet -> GameState -> Comet
handleCometCollision c gstate@GameState{..}     | checkCollision c bullets || collide c player = c {cLives = cLives c - 1}
                                                | otherwise = c

handleScatterCollision :: Scatter -> GameState -> Scatter
handleScatterCollision s gstate@GameState{..}  | checkCollision s bullets  || collide s player= s {sLives = sLives s - 1}
                                               | otherwise = s


handleUfoCollision :: UFO -> GameState -> UFO
handleUfoCollision u gstate@GameState{..}    | invicibel == False || checkCollision u bullets  || collide u player = u {uLives = uLives u - 1}
                                             | otherwise = u


deadScatter :: Scatter -> [Comet] -> [Comet]
deadScatter s@Scatter{..} c = c ++ [b1, b2, b3, b4]
    where 
        sLoc = sLocation          -- Assuming sLocation is a function or field that gets location from Scatter
        sDir = sDirection       -- Assuming sDirection is a vector (x, y)

        -- Calculate new directions by rotating sDir
        b1Dir = rotateVector sDir (-45)
        b2Dir = rotateVector sDir 45
        b3Dir = rotateVector sDir (-90)
        b4Dir = rotateVector sDir 90

        -- Define each comet with modified location and direction
        b1 = Comet 1 sLocation b1Dir sFacing ( color yellow $ polygon [(0,0), (0,30), (30,30),(30,0)]) 2  BB{ centerX = fst (sLoc) + 15, centerY = snd (sLoc) + 15, halfWidth = 15, halfHeigth = 15, rotation = 90}
        b2 = Comet 1 sLocation b2Dir sFacing ( color yellow $ polygon [(0,0), (0,30), (30,30),(30,0)]) 2  BB{ centerX = fst (sLoc) + 15, centerY = snd (sLoc) + 15, halfWidth = 15, halfHeigth = 15, rotation = 90}
        b3 = Comet 1 sLocation b3Dir sFacing ( color yellow $ polygon [(0,0), (0,30), (30,30),(30,0)]) 2  BB{ centerX = fst (sLoc) + 15, centerY = snd (sLoc) + 15, halfWidth = 15, halfHeigth = 15, rotation = 90}
        b4 = Comet 1 sLocation b4Dir sFacing ( color yellow $ polygon [(0,0), (0,30), (30,30),(30,0)]) 2  BB{ centerX = fst (sLoc) + 15, centerY = snd (sLoc) + 15, halfWidth = 15, halfHeigth = 15, rotation = 90}
   

