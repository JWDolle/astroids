
{-# LANGUAGE RecordWildCards #-}
module Projectile where
import Graphics.Gloss
import BoundingBox
import Entity
import Constants


data Projectile = Projectile{
    pShape :: Picture
    ,pBB :: BoundingBox
    ,pLocation :: Point
    ,pDirection :: Vector
}





spawnBullet :: Projectile  -> Bullets -> Bullets
spawnBullet x xs | length xs < 3 = x : xs
                 | otherwise =  xs



type Lasers = [Projectile]
type Bullets = [Projectile]
instance Moveable Projectile where
    move p@Projectile{..} = p { 
        pLocation = adjusted,
        pBB = updatedBB
    }
        where 
            adjusted = (fst pLocation + fst pDirection * bulletSpeed,
                        snd pLocation + snd pDirection * bulletSpeed)
                  -- Compute the change in position
            (dx, dy) = (fst adjusted - fst pLocation, snd adjusted - snd pLocation)

            -- Update the bounding box based on movement
            updatedBB = updateBoundingBox (dx, dy) (rotation pBB) pBB
   
    rotate_ p = p
instance HasBounding Projectile where
    getBB p@Projectile{..} = pBB


