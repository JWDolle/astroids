
module BoundingBox where
import Constants
import Entity
import Graphics.Gloss

--- WE USE A ORIENTED BOUNDING BOX ----
data BoundingBox
    =  BB { 
            centerX :: Float
            ,centerY :: Float
            , halfWidth :: Float
            , halfHeight :: Float
            , rotation :: Float 
      } 
    deriving (Show, Eq)

bbCorners :: BoundingBox -> [Point]
bbCorners (BB cx cy hw hh r) =
    let cosR = cos (radians r)
        sinR = sin (radians r)
        corners = [
            (cx - hw * cosR - hh * sinR, cy - hw * sinR + hh * cosR),  -- Bottom left
            (cx + hw * cosR - hh * sinR, cy + hw * sinR + hh * cosR),  -- Bottom right
            (cx + hw * cosR + hh * sinR, cy + hw * sinR - hh * cosR),  -- Top right
            (cx - hw * cosR + hh * sinR, cy - hw * sinR - hh * cosR)  ] -- Top Left
    in corners

projectPoint :: (Float, Float) -> (Float, Float) -> Float
projectPoint (x, y) (ax, ay) = (x * ax + y * ay)

-- Function to project the corners of an OBB onto an axis
projectBB :: BoundingBox -> (Float, Float) -> (Float, Float)
projectBB obb axis =
    let corners = bbCorners obb
        projections = map (`projectPoint` axis) corners
        minProj = minimum projections
        maxProj = maximum projections
    in (minProj, maxProj)

overlap :: (Float, Float) -> (Float, Float) -> Bool
overlap (min1, max1) (min2, max2) = 
    not (max1 < min2 || max2 < min1)
                 
collide :: BoundingBox -> BoundingBox -> Bool
collide bb1 bb2    =    
    let 
        angle1 = radians $ rotation bb1
        angle2 = radians $ rotation bb2
        axes1 = [(cos (angle1), sin (angle1)),
                 (-sin (angle1), cos (angle1))]
        axes2 = [(cos (angle2), sin angle2),
                 (-sin angle2, cos angle2)]
        axes = axes1 ++ axes2
        projections1 = map (projectBB bb1) axes
        projections2 = map (projectBB bb2) axes
    in all (uncurry overlap) (zip projections1 projections2)


updateBoundingBox :: Vector -> Float -> BoundingBox -> BoundingBox
updateBoundingBox (dx, dy) newRotation bb = bb {
    centerX = correctedCenterX bb dx, 
    centerY = correctedCenterY bb dy,  -- Update center coordinates
    rotation = newRotation          -- Update rotation
} where
    correctedCenterX bb dx | (centerX bb) + dx > (fromIntegral screenSize) / 2 = (centerX bb) + dx - fromIntegral screenSize
                           | (centerX bb) + dx < -((fromIntegral screenSize) / 2) = (centerX bb) + dx + fromIntegral screenSize
                           | otherwise = (centerX bb) + dx
    correctedCenterY bb dy | (centerY bb) + dy > (fromIntegral screenSize) / 2 = (centerY bb) + dy - fromIntegral screenSize
                           | (centerY bb) + dy < -((fromIntegral screenSize) / 2) = (centerY bb) + dy + fromIntegral screenSize
                           | otherwise = (centerY bb) + dy

     


