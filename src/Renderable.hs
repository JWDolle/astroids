--We will have a Renderable type class which given an entity, returns a Picture
{-# LANGUAGE RecordWildCards #-}
module Renderable where
import Graphics.Gloss 
import Constants
import Entity
import Player
import Enemy


-- Type class to render a picture
class Renderable a where
    render :: a -> Picture

instance Renderable Player where
    render player = 
        let (x, y) = pLocation player
            nAngle = (-1) * extractAngle (pMovedir player) * (180 / pi)  -- Convert radians to degrees

            -- Center of the shape (the triangle's geometric center)
            shapeCenterX = 15  -- Adjust this based on your shape's actual geometry
            shapeCenterY = 15
        in translate x y  -- Step 3: Move to player's position (after rotation)
           . rotate nAngle  -- Step 2: Rotate the shape around the origin (centered shape)
           . translate (-shapeCenterX) (-shapeCenterY)  -- Step 1: Move shape center to (0,0)
           $ pShape player  -- Finally, render the shape


instance Renderable Enemy where
    render (C c@Comet {..}) =
        let (x, y) = cLocation
            nAngle = (-1) * extractAngle cFacing * (180 / pi)  -- Convert radians to degrees
            
            -- Center of the shape (adjust these based on your shape's actual geometry)
            shapeCenterX = 30  
            shapeCenterY = 30  
        in translate x y  -- Move to comet's position
           . rotate nAngle  -- Rotate the shape around its center
           . translate (-shapeCenterX) (-shapeCenterY)  -- Move shape center to (0, 0)
           $ cShape  -- Render the shape
-- Flips a picture over the y-axis
flipPicture :: Picture -> Picture
flipPicture = scale (-1) (1)

-- Scales a picture to the size of a bounding box
--scaleToBoundingBox :: Picture -> BoundingBox -> Picture

-- Translates a picture to the position of a boundingbox
--translateToBoundingBox :: Picture -> BoundingBox 