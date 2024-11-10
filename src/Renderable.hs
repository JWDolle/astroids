--We will have a Renderable type class which given an entity, returns a Picture
{-# LANGUAGE RecordWildCards #-}
module Renderable where
import Graphics.Gloss 
import Constants
import Entity
import Player
import Enemy
import BoundingBox
import Projectile
import Button
import Animation


-- Type class to render a picture
class Renderable a where
    render :: a -> Picture

-- Function to execute a sequence of transformations
transformations :: Point -> Vector -> Float -> Float -> Picture -> Picture
transformations local@(x,y) dir cX cY pic = translate (x + cX) (y + cY)
                                           . rotate nAngle
                                           . translate (-cX) (-cY)
                                           $ pic
        where 
            nAngle = (-1) * degrees(extractAngle dir)
-- Renderable for the player
instance Renderable Player where
    render player@Player{..} | isMoving = transformations pLocation pFacing (playerWidth / 2) (playerHeigth / 2) $ renderAnimation animation
                             | otherwise = transformations pLocation pFacing (playerWidth / 2) (playerHeigth / 2) pShape
-- Renderable for the comet enemy
instance Renderable Comet where
    render comet@Comet{..} = transformations cLocation cFacing (halfWidth cBB) (halfHeigth cBB) cShape

-- Renderable for the scatter enemy
instance Renderable Scatter where
    render scatter@Scatter{..} = transformations sLocation sFacing (halfWidth sBB) (halfHeigth sBB) sShape

-- Renderable for the UFO enemy
instance Renderable UFO where
    render ufo@UFO{..} = transformations uLocation uFacing (halfWidth uBB) (halfHeigth uBB) uShape

-- Renderable for projectiles
instance Renderable Projectile where
    render projectile@Projectile {..} = transformations prLocation prDirection (projectileWidth/2) (projectileWidth/2) prShape

-- Renderable for the buttons
instance Renderable Button where
    render button@Button {..} = transformations bLocation (0,0) (playWidth/2) (playHeigth/2) bShape

-- Render for all bullets
renderBullets :: Bullets -> Picture
renderBullets b = pictures $ map render b
   
-- Render for all lasers
renderLasers:: Lasers -> Picture
renderLasers l = pictures $ map render l
-- Flips a picture over the y-axis
flipPicture :: Picture -> Picture
flipPicture = scale (-1) (1)
