{-# LANGUAGE RecordWildCards #-}

module Player where
import Graphics.Gloss
import Entity
import Constants


data Moving = STOP | MOVING
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
                }
p1 :: Player
p1 = Player {
                pName = "player 1"
                , pLives = 3
                , pLocation = (0,0)
                , pMovedir = (0,1)
                , pShootdir = (0,1) -- this one we might need later for turning while decending
                , pShape = color blue $ scale 10 10 $ polygon[(0,0), (3,0), (3,3), (0,3)]
                , pSpeed = 0 
                , pMaxSpd = 10
                , isMoving = False    
                , isRotatingL = False    
                , isRotatingR = False
                , pAccel = 0.1
                , pDecel = 0.1
                , isDecelling = False
            }



-- moveable objects are things that can rotate and move 
instance Moveable Player where
    move p@Player {..} = p { 
        pLocation = adjusted,  -- Update the player's location
        pSpeed = newSpd        -- Update speed based on acceleration or deceleration
    }
        where 
            newSpd | isMoving     = min (pSpeed + pAccel) pMaxSpd  -- Accelerate to max speed
                | isDecelling  = max (pSpeed - pDecel) 0  -- Decelerate
                | otherwise    = pSpeed  -- Maintain current speed

            adjusted = (fst pLocation + fst pMovedir * newSpd, 
                        snd pLocation + snd pMovedir * newSpd)

    rotate_ p@Player {..} = p {
        pMovedir = adjusted
    }
        where
            -- Define the angle of rotation (for example, 5 degrees)
            angleRadians = radians rAngle -- Example fixed rotation angle of 5 degrees
            -- Extract current angle from pMovedir
            cAngle = extractAngle pMovedir
            -- Adjust angle based on rotation direction (left or right)
            nAngle 
                | isRotatingL = cAngle - angleRadians  -- Anti-clockwise
                | isRotatingR = cAngle + angleRadians  -- Clockwise
                | otherwise   = cAngle  -- No rotation
                -- Compute new direction vector using cos and sin of the new angle
            adjusted = (cos nAngle, sin nAngle)

