-- Projectile code goes here

{-# LANGUAGE RecordWildCards #-}
module Projectile where
import Graphics.Gloss
import BoundingBox
import Entity
import Constants




data Bullet = Bullet {
    bShape :: Picture
    ,bBB :: BoundingBox
    ,bLocation :: Point
    ,bDirection :: Vector
    
} 
data Laser = Laser {
    lShape :: Picture
    ,lBB :: BoundingBox
    ,lLocation :: Point
    ,lDirection :: Vector
}

type Bullets = [Bullet]
type Lasers = [Laser]

addBullet :: Bullet -> Bullets -> Bullets
addBullet x xs = if (length xs) < 3 then x:xs
                 else xs

instance Moveable Bullet where
    move b@(Bullet {..}) = b { 
        bLocation = adjusted,
        bBB = updatedBB
    }
        where 
            adjusted = (fst bLocation + fst bDirection * bulletSpeed,
                        snd bLocation + snd bDirection * bulletSpeed)
                  -- Compute the change in position
            (dx, dy) = (fst adjusted - fst bLocation, snd adjusted - snd bLocation)

            -- Update the bounding box based on movement
            updatedBB = updateBoundingBox (dx, dy) (rotation bBB) bBB
   
    rotate_ b = b