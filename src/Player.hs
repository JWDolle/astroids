{-# LANGUAGE RecordWildCards #-}

module Player where
import Graphics.Gloss
import Entity
import Constants
import BoundingBox
import Animation




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
                , pFacing :: Vector
                , pShape :: Picture
                , pSpeed :: Float
                , isMoving :: Bool
                , isRotatingL :: Bool
                , isRotatingR :: Bool
                , isDecelling:: Bool
                , animation:: Animation
                , bb :: BoundingBox
                , invincible :: Bool
                }




maxSpeed :: Float
maxSpeed = 5

player1Local:: Point
player1Local = (0,0)



p1 :: Player
p1 = Player {
                pName = "player 1"
                , pLives = 10
                , pLocation = player1Local -- center of the player
                , pMovedir = (0,1)
                , pFacing = (0,1)
                , pShape = color blue  $ polygon [(0,0), (30,15), (0,30)]
                , pSpeed = 0 
                , isMoving = False    
                , isRotatingL = False    
                , isRotatingR = False
                , isDecelling = False
                , animation = (Animate 0 0 (map (color blue) [polygon[(0,0), (30,15), (0,30)], pictures [polygon[(0,0), (30,15), (0,30)], polygon[(-5,8), (-15,15), (-5,22)]], pictures [polygon[(0,0), (30,15), (0,30)], polygon[(-5,8), (-20,15), (-5,22)]], pictures [polygon[(0,0), (30,15), (0,30)], polygon[(-5,8), (-15,15), (-5,22)]]]) True)
                , bb = BB{centerX = (fst player1Local) + playerWidth/2 , centerY = (snd player1Local) + playerHeigth/2, halfWidth = playerWidth/2, halfHeigth = playerHeigth/2, rotation = 90}

            }

instance HasBounding Player where
    getBB p@Player {..} = bb    


-- moveable objects are things that can rotate and move 
instance Moveable Player where
    move p@Player {..} = p { 
        pLocation = (centerX newbb - 15, centerY newbb - 15),  -- Update the player's location
        pMovedir = newMovedir,  -- Update movement direction when isMoving is true
        pSpeed = newSpd,        -- Update speed based on acceleration or deceleration
        bb = newbb,
        isDecelling = newIsDecelling
    }
        where 
            -- Update speed based on moving or decelerating
            newSpd  | isMoving     = min (pSpeed + accel) maxSpeed  -- Accelerate to max speed
                    | isDecelling  = max (pSpeed - decel) 0  -- Decelerate
                    | otherwise    = pSpeed  -- Maintain current speed

            -- Set pMovedir to match pFacing if isMoving is true
            newMovedir = if isMoving then normalize pFacing else pMovedir

            -- Adjust position only based on initial movement direction when decelerating
            adjusted = if isDecelling
                          then (fst pLocation + fst pMovedir * newSpd, 
                                snd pLocation + snd pMovedir * newSpd)
                          else (fst pLocation + fst newMovedir * newSpd, 
                                snd pLocation + snd newMovedir * newSpd)

            (dx, dy) = (fst adjusted - fst pLocation, snd adjusted - snd pLocation)

            newbb | isMoving     = updateBoundingBox (dx, dy) newRotation  bb  -- Move and rotate
                  | isDecelling  = updateBoundingBox (dx, dy) newRotation  bb  -- Decelerate and rotate
                  | otherwise    = bb  -- No movement, keep the bounding box unchanged  -- Maintain current speed
            
            -- Rotate the bounding box based on `pFacing` angle regardless of movement state
            newRotation = degrees $ extractAngle $ normalize pFacing
            --newbb = updateBoundingBox (dx, dy) newRotation bb

            -- Check if we should keep decelerating
            newIsDecelling = newSpd > 0 

    rotate_ p@Player{..} = p {
        pFacing = normalized,  -- Update player's facing direction
        pMovedir = if isDecelling then pMovedir else normalized,  -- Only change direction if not decelerating
        bb = updatedBB        -- Update the player's bounding box rotation
    }
        where
            angleRadians = radians rAngle  -- Fixed rotation angle (for example, 5 degrees)
            cAngle = extractAngle pFacing
            nAngle 
                | isRotatingL = cAngle - angleRadians  -- Anti-clockwise
                | isRotatingR = cAngle + angleRadians  -- Clockwise
                | otherwise   = cAngle  -- No rotation

            adjusted = (cos nAngle, sin nAngle)
            normalized = normalize adjusted

            degAngle = degrees (extractAngle normalized)
            updatedBB = updateBoundingBox (0,0) degAngle bb  -- Rotate bounding box continuously
