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
transformations :: Point -> Vector -> Float -> Float -> Picture -> Picture
transformations local@(x,y) dir cX cY pic= translate (x + cX) (y + cY)
                                           . rotate nAngle
                                           . translate (-cX) (-cY)
                                           $ pic
        where 
            nAngle = (-1) * degrees(extractAngle dir)

instance Renderable Player where
    render player@Player{..} = transformations pLocation pFacing (playerWidth / 2) (playerHeigth / 2) pShape

instance Renderable Comet where
    render comet@Comet{..} = transformations cLocation cFacing (halfWidth cBB) (halfHeigth cBB) cShape

instance Renderable Scatter where
    render scatter@Scatter{..} = transformations sLocation sFacing (halfWidth sBB) (halfHeigth sBB) sShape

instance Renderable Projectile where
    render projectile@Projectile {..} = transformations prLocation prDirection 5 5 prShape

instance Renderable Button where
    render button@Button {..} = transformations bLocation (0,0) (playWidth/2) (playHeigth/2) bShape

renderBullets :: Bullets -> Picture
renderBullets b = pictures $ map render b
   
renderLasers:: Lasers -> [Picture]
renderLasers l = map render l
-- Flips a picture over the y-axis
flipPicture :: Picture -> Picture
flipPicture = scale (-1) (1)

-- Scales a picture to the size of a bounding box
--scaleToBoundingBox :: Picture -> BoundingBox -> Picture

-- Translates a picture to the position of a boundingbox
--translateToBoundingBox :: Picture -> BoundingBox 