
{-# LANGUAGE RecordWildCards #-}
module Projectile where
import Graphics.Gloss
import BoundingBox
import Entity
import Constants


data Projectile = Projectile{
    prShape :: Picture
    ,prBB :: BoundingBox
    ,prLocation :: Point
    ,prDirection :: Vector
}





spawnBullet :: Projectile  -> Bullets -> Bullets
spawnBullet x xs | length xs < 3 = x : xs
                 | otherwise =  xs



type Lasers = [Projectile]
type Bullets = [Projectile]

instance Moveable Projectile where
    move p@Projectile{..} = p { 
        prLocation = adjusted,
        prBB = updatedBB
    }
        where 
            adjusted = (fst prLocation + fst prDirection * bulletSpeed,
                        snd prLocation + snd prDirection * bulletSpeed)
                  -- Compute the change in position
            (dx, dy) = (fst adjusted - fst prLocation, snd adjusted - snd prLocation)

            -- Update the bounding box based on movement
            updatedBB = updateBoundingBox (dx, dy) (rotation prBB) prBB
   
    rotate_ p = p
instance HasBounding Projectile where
    getBB p@Projectile{..} = prBB


