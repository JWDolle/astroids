{-# LANGUAGE RecordWildCards #-}

module Player where
import Graphics.Gloss
import Entity
import Constants
import BoundingBox


data Moving = STOP | MOVING

playerWidth :: Float
playerWidth = 30

playerHeigth :: Float
playerHeigth= 30



-- if we want to have turning during 
data Player = Player {
                  pName :: String
                , pLives :: Int
                , pLocation :: Point
                , pMovedir :: Vector
                , pShootdir:: Vector
                , pShape :: Picture
                , pSpeed :: Float
                , isMoving :: Bool
                , isRotatingL :: Bool
                , isRotatingR :: Bool
                , isDecelling:: Bool
                , bb :: BoundingBox
                , pShoot:: Point
                }

getPlayerBB :: Player -> BoundingBox
getPlayerBB p = bb p


maxSpeed :: Float
maxSpeed = 5

player1Local:: Point
player1Local = (0,0)

accel:: Float
accel = 0.1

decel :: Float
decel = 0.1

p1 :: Player
p1 = Player {
                pName = "player 1"
                , pLives = 3
                , pLocation = player1Local -- center of the player
                , pMovedir = (0,1)
                , pShootdir = (0,1) -- this one we might need later for turning while decending
                , pShape = color blue  $ polygon[(0,0), (30,0), (30,30), (0,30)]
                , pSpeed = 0 
                , isMoving = False    
                , isRotatingL = False    
                , isRotatingR = False
                , isDecelling = False
                , bb = BB{centerX = (fst player1Local) + playerWidth/2 , centerY = (snd player1Local) + playerHeigth/2, halfWidth = playerWidth/2, halfHeigth = playerHeigth/2, rotation = 90}

            }

instance HasBounding Player where
    getBB p@Player {..} = bb    


-- moveable objects are things that can rotate and move 
instance Moveable Player where
    move p@Player {..} = p { 
        pLocation = adjusted,  -- Update the player's location
        pSpeed = newSpd,        -- Update speed based on acceleration or deceleration
        bb = newbb,
        isDecelling = newIsDecelling
    }
        where 
            newSpd  | isMoving     = min (pSpeed + accel) maxSpeed  -- Accelerate to max speed
                    | isDecelling  = max (pSpeed - decel) 0  -- Decelerate
                    | otherwise    = pSpeed  -- Maintain current speed

       
            adjusted = (fst pLocation + fst pMovedir * newSpd, 
                        snd pLocation + snd pMovedir * newSpd)

            (dx, dy) = (fst adjusted - fst pLocation, snd adjusted - snd pLocation)
            newRotation = degrees $ atan2 (snd pMovedir) (fst pMovedir)
            newbb | isMoving     = updateBoundingBox (dx, dy) newRotation bb  -- Move and rotate
                  | isDecelling  = updateBoundingBox (dx, dy) newRotation bb  -- Decelerate and rotate
                  | otherwise    = bb  -- No movement, keep the bounding box unchanged  -- Maintain current speed
            
            newIsDecelling | newSpd > 0 = True
                           | otherwise = False
    rotate_ p@Player{..} = p {
        pMovedir = normalized,  -- Update player's direction
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
            normalized = normalize adjusted
            -- Update the bounding box with the new rotation
            degAngle = degrees (extractAngle normalized)
            updatedBB = updateBoundingBox (0,0) degAngle bb


