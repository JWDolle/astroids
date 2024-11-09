{-# LANGUAGE RecordWildCards #-}



module Updates where
import Player
import Model
import Enemy
import Entity
import BoundingBox
import Projectile
import Random
import System.Random
import Constants
import Animation
import Player (Player)


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

updateAnimationPlayer :: Player -> Player
updateAnimationPlayer p@Player{animation = a} = p{animation = updateAnimation a}

updateEnemies ::  GameState -> GameState
updateEnemies gstate@GameState{..} = if length scatters > 2 then gstate{comets = map (\x -> rotate_ (move x)) comets,
                                                  scatters = map (\x -> rotate_ (move x))scatters,
                                                  ufos = map updateUfo ufos} else spawnScatter scat gstate
                        where 
                            updateUfo:: UFO -> UFO
                            updateUfo u | (invincible u) = u
                                        | otherwise = move u

spawnComet :: Comet -> GameState -> GameState
spawnComet (Comet  liv loc dir f sh sp  bb) gState@(GameState i e p c u s l b r sc Playing) = (GameState i e p (newCom:c) u s l b (snd y) sc Playing)
    where

        randomLocationX = randomRange (0,screenSize) r
        x = validSpawn ((fromIntegral (fst randomLocationX)) - ((fromIntegral screenSize) / 2)) (fst (pLocation p)) (snd randomLocationX)
        randomLocationY = randomRange (0,screenSize) (snd x)
        y = validSpawn ((fromIntegral (fst randomLocationY)) - ((fromIntegral screenSize) / 2)) (snd (pLocation p)) (snd randomLocationY)

        randomDirectionX = randomRange (0,2) (snd randomLocationY)
        randomDirectionY = randomRange (0,2) (snd randomDirectionX)

        newbb = bb{
            centerX = fst x,
            centerY = fst y
        }

        newCom = (Comet  liv (fst x, fst y) ((fst randomDirectionX) - 1, (fst randomDirectionY) - 1) f sh sp newbb)

spawnScatter :: Scatter -> GameState -> GameState
spawnScatter (Scatter nam liv loc dir f sh sp  bb) gState@(GameState i e p c u s l b r sc Playing) = (GameState i e p c u (newscat:s) l b (snd y) sc Playing)
    where

        randomLocationX = randomRange (0,screenSize) r
        x = validSpawn ((fromIntegral (fst randomLocationX)) - ((fromIntegral screenSize) / 2)) (fst (pLocation p)) (snd randomLocationX)
        randomLocationY = randomRange (0,screenSize) (snd x)
        y = validSpawn ((fromIntegral (fst randomLocationY)) - ((fromIntegral screenSize) / 2)) (snd (pLocation p)) (snd randomLocationY)

        randomDirectionX = randomRange (0,2) (snd randomLocationY)
        randomDirectionY = randomRange (0,2) (snd randomDirectionX)

        newbb = bb{
            centerX = fst x,
            centerY = fst y
        }

        newscat = (Scatter nam liv (fst x, fst y) ((fst randomDirectionX) - 1, (fst randomDirectionY) - 1) f sh sp newbb)

spawnUFO :: UFO -> GameState -> GameState
spawnUFO (UFO nam liv loc dir sh bb inv) gState@(GameState i e p c u s l b r sc Playing) = (GameState i e p c (newUFO:u) s l b (snd loc) sc Playing)
    where
        
        wall :: (Int, StdGen)
        wall = randomRange (1,4) r
        randomLocation = randomRange (0,screenSize) (snd wall)
        loc = validSpawn ((fromIntegral (fst randomLocation)) - ((fromIntegral screenSize) / 2)) (fst (pLocation p)) (snd randomLocation)

        x = case fst wall of
            1 -> 0
            2 -> 0
            3 -> fst loc
            4 -> -(fst loc)

        y = case fst wall of            
            1 -> fst loc
            2 -> -(fst loc)
            3 -> 0
            4 -> 0

        newbb = bb{
            centerX = x,
            centerY = y
            }

        newUFO = (UFO nam liv ( x,  y) (pLocation p) sh newbb inv)

validSpawn :: Float -> Float -> StdGen -> (Float, StdGen)
validSpawn e p rand | withinWrap e p = validSpawn (fromIntegral (fst newRand)) p (snd newRand)
                    | otherwise = (e,rand)
    where
        newRand = randomRange (0,screenSize) rand
        withinWrap :: Float -> Float -> Bool
        withinWrap a b = (a + spawningRadius > b && a - spawningRadius < b) || (a + spawningRadius > b + fromIntegral screenSize && a - spawningRadius < b + fromIntegral screenSize) || (a + spawningRadius > b - fromIntegral screenSize && a - spawningRadius < b - fromIntegral screenSize)


updatePlayer:: GameState -> GameState
updatePlayer gstate@(GameState _ _ p _ _ _ _ _ _ _ _)= 
    let
        movedPlayer = updateMovementPlayer (updateAnimationPlayer p) gstate
        rotatedPlayer = updateRotationPlayer (player movedPlayer) movedPlayer
        updatedPlayer = rotatedPlayer
    in  updatedPlayer

updateBullets:: Float -> GameState -> GameState
updateBullets secs gstate@(GameState _ _ _ _ _ _ _ b _ _ _) =
    let 
        timedOut = filterProjectiles (map (\x -> outOfTime x secs ) b)
        movedBullets = map move timedOut
        updatedBullets = gstate{ bullets = filterProjectiles movedBullets }
    in  updatedBullets






 