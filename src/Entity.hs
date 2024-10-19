{-# LANGUAGE RecordWildCards #-}
-- Entity code goes here (shared code for player, enemies and projectiles)
module Entity where


import Graphics.Gloss

pVelocity :: Float
pVelocity = 2;

rAngle :: Float
rAngle = 15

data Moving = STOP | MOVING
-- if we want to have turning during 
data Player = Player {
                  pName :: String
                , pLives :: Int 
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
                , pAccel = 0.5
                , pDecel = 0.1
                , isDecelling = False
            }






class Moveable a where
    move:: a -> a
    rotate_::  a -> a


-- moveable objects are things that can rotate and move 
instance Moveable Player where
    move p@Player {..} = p { 
        pLocation = adjusted  -- Update the player's location
        ,pSpeed = newSpd
    }
        where 
           
            newSpd | isMoving = min (pSpeed + pAccel) pMaxSpd  -- Accelerate to max speed
                   | otherwise = max (pSpeed - pDecel) 0
            adjusted | isMoving = (fst pLocation + fst pMovedir * newSpd, 
                                    snd pLocation + snd pMovedir * newSpd)

    rotate_ p@Player {..} = p {
        pMovedir = adjusted
    }
        
        where
            -- Define the angle of rotation (you need to pass an angle, e.g., 5 degrees)
            angleRadians = radians rAngle -- example fixed rotation angle of 5 degrees
            -- Extract current angle from pDirection
            cAngle = extractAngle pMovedir
            -- Adjust angle based on rotation direction (L or R)
            nAngle 
              | isRotatingL = cAngle - angleRadians  -- Anti-clockwise (increase angle)
              | isRotatingR = cAngle + angleRadians  -- Clockwise (decrease angle)
              | otherwise   = cAngle  -- No rotation
            -- Compute new direction vector using cos and sin of the new angle
            adjusted = (cos nAngle, sin nAngle)


radians:: Float -> Float
radians d = d * (pi/180)

extractAngle :: Vector -> Float
extractAngle v = atan2 (snd v) (fst v)



            
