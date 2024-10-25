-- Enemy specific code goes here
{-# LANGUAGE RecordWildCards #-}


module Enemy where
import Graphics.Gloss
import Entity
import Constants
import BoundingBox



---Data types


    
data Scatter = Scatter { --become small comets
    sName:: String
    ,sLives :: Int
    ,sLocation :: Point
    ,sDirection :: Vector
    ,sSHape:: Picture
    ,sBB :: BoundingBox
}

data UFO = UFO { --shoots the player
    uName :: String
    ,uLives :: Int
    ,uLocation :: Point
    ,uDirection :: Vector
    ,uShape :: Picture
    ,uBB :: BoundingBox

}
data Comet = Comet { -- no intelligence
    cName:: String
    ,cLives :: Int
    ,cLocation :: Point
    ,cDirection :: Vector
    ,cFacing :: Vector
    ,cShape :: Picture
    ,cSpeed :: Float
    ,cRotateL :: Bool
    ,cBB :: BoundingBox

}


instance Moveable Comet where
    move c@Comet{..} =  c { 
        cLocation = (centerX updatedBB - 30, centerY updatedBB - 30),
        cBB = updatedBB
    }
        where 
            adjusted = (fst cLocation + fst cDirection * cSpeed,
                        snd cLocation + snd cDirection * cSpeed)
                  -- Compute the change in position
            (dx, dy) = (fst adjusted - fst cLocation, snd adjusted - snd cLocation)

            -- Update the bounding box based on movement
            updatedBB = updateBoundingBox (dx, dy) (rotation cBB)  cBB

    rotate_ c@Comet {..} = c {
        cFacing = normalized,
        cBB = updatedBB
    }
        where
            angleRadians = radians c_rAngle
            cAngle = extractAngle cFacing
            nAngle = if cRotateL then cAngle - angleRadians else cAngle + angleRadians
            adjusted = (cos nAngle, sin nAngle)
            normalized = normalize adjusted
         -- Update the bounding box with new rotation
            updatedBB = updateBoundingBox (0, 0) (degrees nAngle)  cBB
     

instance Moveable  UFO where
    move  u@UFO {..} = u {
        uLocation = adjusted,
        uBB = updatedBB
    }
        where
            adjusted = (fst uLocation + fst uDirection * 2,
                        snd uLocation + snd uDirection * 2)
              -- Compute the change in position
            (dx, dy) = (fst adjusted - fst uLocation, snd adjusted - snd uLocation)

            -- Update the bounding box based on movement
            updatedBB = updateBoundingBox (dx, dy) (rotation uBB)  uBB
    rotate_ u@UFO {..} = u {
        uDirection = normalized,
        uBB = updatedBB
    }
        where
            angleRadians = radians 10
            uAngle = extractAngle uDirection
            nAngle = uAngle + angleRadians
            adjusted = (cos nAngle, sin nAngle)
         -- Update the bounding box with new rotation
            normalized = normalize adjusted
            updatedBB = updateBoundingBox (0, 0) (degrees nAngle)  uBB

instance Moveable Scatter where
    move s@Scatter {..} =  s {
        sLocation = adjusted,
        sBB = updatedBB
    }
        where
            adjusted = (fst sLocation + fst sDirection * 1.5,
                        snd sLocation + snd sDirection * 1.5)
              -- Compute the change in position
            (dx, dy) = (fst adjusted - fst sLocation, snd adjusted - snd sLocation)

            -- Update the bounding box based on movement
            updatedBB = updateBoundingBox (dx, dy) (rotation sBB)  sBB

 
    rotate_ s@Scatter {..} = s { 
        sDirection = normalized,
        sBB = updatedBB
    }
        where
            angleRadians = radians 5
            sAngle = extractAngle sDirection
            nAngle = sAngle + angleRadians
            adjusted = (cos nAngle, sin nAngle)
            normalized = normalize adjusted
            -- Update the bounding box with new rotation
            updatedBB = updateBoundingBox (0, 0) (degrees nAngle) sBB
    
instance HasBounding Comet where
    getBB c@Comet{..} = cBB

instance HasBounding UFO where
    getBB u@UFO{..} = uBB
   
instance HasBounding Scatter where
    getBB s@Scatter{..} = sBB
 




----EXAMPLE ENIMIES

c1 :: Comet
c1 = Comet {
    cName = "Test comet"
    ,cLives = 5
    ,cLocation = (-100, 100) -- center of the thing 
    ,cDirection = (1,0)
    ,cFacing = (1, 0)
    ,cShape = color red $ polygon [(0,0), (0,60), (60,60),(60,0)]
    ,cSpeed = 1
    ,cRotateL = True
    ,cBB = BB{ centerX = -70, centerY = 130, halfWidth = 30, halfHeigth = 30, rotation = 90}
    
}