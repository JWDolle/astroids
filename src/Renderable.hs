--We will have a Renderable type class which given an entity, returns a Picture

module Renderable where
import Graphics.Gloss 
import Constants

-- Type class to render a picture
class Renderable a where
    render :: a -> Picture

-- Flips a picture over the y-axis
flipPicture :: Picture -> Picture
flipPicture = scale (-1) (1)

-- Scales a picture to the size of a bounding box
--scaleToBoundingBox :: Picture -> BoundingBox -> Picture

-- Translates a picture to the position of a boundingbox
--translateToBoundingBox :: Picture -> BoundingBox -> Picture