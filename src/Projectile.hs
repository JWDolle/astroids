
{-# LANGUAGE RecordWildCards #-}
module Projectile where
import Graphics.Gloss
import BoundingBox
import Entity
import Player

import Constants 


data Projectile = Projectile{
    prShape :: Picture
    ,prBB :: BoundingBox
    ,prLocation :: Point
    ,prDirection :: Vector
    ,prAlive :: Bool
    ,time :: Int
}





spawnBullet :: Projectile  -> Bullets -> Bullets
spawnBullet x xs | length xs < shootCooldown = x : xs
                 | otherwise =  xs

outOfTime :: Projectile -> Float -> Projectile
outOfTime b secs | time b > bulletExistance = b {prAlive = False}
                 | otherwise = b

type Lasers = [Projectile]
type Bullets = [Projectile]
createbullet ::  Player -> Projectile
createbullet  p@Player{..} =
    let
        dirOff | isDecelling = (fst pFacing * playerWidth/2, snd pFacing * playerHeigth/2)
               | otherwise = (fst pMovedir * playerWidth/2, snd pMovedir * playerHeigth/2)
        ofset = (fst pLocation + (playerWidth/2) + fst dirOff - 5, snd pLocation + (playerHeigth/2) + snd dirOff -5)
        bulletLocation = ofset
        newRotation | isDecelling = degrees (extractAngle pFacing) 
                    | otherwise = degrees (extractAngle pMovedir)
        direction   | isDecelling = pFacing
                    | otherwise = pMovedir
    in   Projectile {
        prShape = color blue $ polygon [(0,0), (0,10), (10,10), (10,0)],
        prBB = BB {
            centerX = fst bulletLocation + 5,
            centerY = snd bulletLocation + 5,
            halfWidth = 5,
            halfHeigth = 5,
            rotation = newRotation -- Use degrees here for the display rotation
        },
        prLocation = bulletLocation,
        prDirection = direction,
        prAlive = True,   
        time = 0

    }


instance Moveable Projectile where
    move p@Projectile{..} = p { 
        prLocation = adjusted,
        prBB = updatedBB,
        time = time  + 1
    }
        where 
            adjusted = (fst prLocation + fst prDirection * bulletSpeed,
                        snd prLocation + snd prDirection * bulletSpeed)
                  -- Compute the change in position
            (dx, dy) = (fst adjusted - fst prLocation, snd adjusted - snd prLocation)

            -- Update the bounding box based on movement
            
            updatedBB = updateBoundingBox (dx, dy) (rotation prBB)  prBB
           

   
    rotate_ p = p
instance HasBounding Projectile where
    getBB p@Projectile{..} = prBB



filterProjectiles :: [Projectile] -> [Projectile]
filterProjectiles l = filter prAlive l