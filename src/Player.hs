{-# LANGUAGE RecordWildCards #-}

module Player where
import Graphics.Gloss
import Entity
import Constants
import BoundingBox


data Moving = STOP | MOVING

playerWidth :: Float
playerWidth = 30

playerHeight :: Float
playerHeight = 30

-- if we want to have turning during 
data Player = Player {
                  pName :: String
                , pLives :: Lives
                , pLocation :: Point
                , pMovedir :: Vector
                , pShootdir:: Vector
                , pShape :: Picture
                , pSpeed :: Float
                , pMaxSpd :: Float
                , isMoving :: Bool
                , isRotatingL :: Bool
                , isRotatingR :: Bool
                , pAccel :: Float
                , pDecel :: Float
                , isDecelling:: Bool
                , bb :: BoundingBox
                }

getPlayerBB :: Player -> BoundingBox
getPlayerBB p = bb p

decLives :: Player -> Player
decLives p = p {pLives = (pLives p) - 1} 

p1 :: Player
p1 = Player {
                pName = "player 1"
                , pLives = 3
                , pLocation = (15,15) -- center of the player
                , pMovedir = (0,1)
                , pShootdir = (0,1) -- this one we might need later for turning while decending
                , pShape = color blue  $ polygon[(0,0), (30,0), (30,30), (0,30)]
                , pSpeed = 0 
                , pMaxSpd = 10
                , isMoving = False    
                , isRotatingL = False    
                , isRotatingR = False
                , pAccel = 0.1
                , pDecel = 0.1
                , isDecelling = False
                , bb = BB{centerX = 15, centerY = 15, halfWidth = 15, halfHeight = 15, rotation = 90}
            }



-- moveable objects are things that can rotate and move 
instance Moveable Player where
    move p@Player {..} = p { 
        pLocation = adjusted,  -- Update the player's location
        pSpeed = newSpd,        -- Update speed based on acceleration or deceleration
        bb = newbb,
        isDecelling = newIsDecelling
    }
        where 
            newSpd  | isMoving     = min (pSpeed + pAccel) pMaxSpd  -- Accelerate to max speed
                    | isDecelling  = max (pSpeed - pDecel) 0  -- Decelerate
                    | otherwise    = pSpeed  -- Maintain current speed

       
            adjusted = (fst pLocation + fst pMovedir * newSpd, 
                        snd pLocation + snd pMovedir * newSpd)

            (dx, dy) = (fst adjusted - fst pLocation, snd adjusted - snd pLocation)
            newRotation = atan2 (snd pMovedir) (fst pMovedir)
            newbb | isMoving     = updateBoundingBox (dx, dy) newRotation bb  -- Move and rotate
                  | isDecelling  = updateBoundingBox (dx, dy) newRotation bb  -- Decelerate and rotate
                  | otherwise    = bb  -- No movement, keep the bounding box unchanged  -- Maintain current speed
            
            newIsDecelling | newSpd > 0 = True
                           | otherwise = False



    rotate_ p@Player {..} = p {
        pMovedir = adjusted,  -- Update player's direction
        bb = updatedBB        -- Update the player's bounding box
        }
        where
            -- Define the angle of rotation (for example, 5 degrees)
            angleRadians = radians rAngle  -- Example fixed rotation angle of 5 degrees
            -- Extract current angle from pMovedir
            cAngle = extractAngle pMovedir
            -- Adjust angle based on rotation direction (left or right)
            nAngle 
                | isRotatingL = cAngle - angleRadians  -- Anti-clockwise
                | isRotatingR = cAngle + angleRadians  -- Clockwise
                | otherwise   = cAngle  -- No rotation
            -- Compute new direction vector using cos and sin of the new angle
            adjusted = (cos nAngle, sin nAngle)

            -- Update the bounding box with the new rotation
            degAngle = extractAngle adjusted
            updatedBB = updateBoundingBox (0, 0) degAngle bb


