-- Enemy specific code goes here
{-# LANGUAGE RecordWildCards #-}


module Enemy where
import Graphics.Gloss
import Entity
import Constants
import BoundingBox
import Random


---Data types    
data Scatter = Scatter { --become small comets
    sName:: String
    ,sLives :: Int
    ,sLocation :: Point
    ,sDirection :: Vector
    ,sFacing :: Vector
    ,sSpeed :: Float
    ,sShape:: Picture
    ,sBB :: BoundingBox
}

data UFO = UFO { --shoots the player
    uLives :: Int
    ,uFacing :: Vector
    ,uLocation :: Point
    ,uDirection :: Vector
    ,uShape :: Picture
    ,uBB :: BoundingBox
    ,invicible :: Bool
    ,aimDir :: Vector
    ,laserCld :: Int

}
data Comet = Comet { -- no intelligence
    cLives :: Int
    ,cLocation :: Point
    ,cDirection :: Vector
    ,cFacing :: Vector
    ,cShape :: Picture
    ,cSpeed :: Float
    ,cBB :: BoundingBox
}



-- Movable instances for each enemy type
instance Moveable Comet where
    move c@Comet{..} =  c { 
        cLocation = (centerX updatedBB - cometWidth/2 , centerY updatedBB - cometHeigth/2),
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
            nAngle =  cAngle + angleRadians
            adjusted = (cos nAngle, sin nAngle)
            normalized = normalize adjusted
         -- Update the bounding box with new rotation
            updatedBB = updateBoundingBox (0, 0) (degrees nAngle)  cBB
     

instance Moveable  UFO where
    move  u@UFO {..} = u {
        uLocation = (centerX updatedBB - scatterWidth/2 , centerY updatedBB - scatterHeigth/2),
        uBB = updatedBB,
        laserCld = updatedCld
    }
        where
            adjusted = (fst uLocation + fst uDirection * 2,
                        snd uLocation + snd uDirection * 2)
              -- Compute the change in position
            (dx, dy) = (fst adjusted - fst uLocation, snd adjusted - snd uLocation)

            -- Update the bounding box based on movement
            updatedBB = updateBoundingBox (dx, dy) (rotation uBB)  uBB
            updatedCld = laserCld + 1
             

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
        sLocation = (centerX updatedBB - scatterWidth/2 , centerY updatedBB - scatterHeigth/2),
        sBB = updatedBB
    }
        where
            adjusted = (fst sLocation + fst sDirection * sSpeed,
                        snd sLocation + snd sDirection * sSpeed)
              -- Compute the change in position
            (dx, dy) = (fst adjusted - fst sLocation, snd adjusted - snd sLocation)

            -- Update the bounding box based on movement
            updatedBB = updateBoundingBox (dx, dy) (rotation sBB)  sBB

 
    rotate_ s@Scatter {..} = s { 
        sFacing = normalized,
        sBB = updatedBB
    }
        where
            angleRadians = radians 5
            sAngle = extractAngle sFacing
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

-- Comet instance
c1 :: Comet
c1 = Comet {
    cLives = 1
    ,cLocation = (0,0) -- center of the thing
    ,cDirection = (1,0)
    ,cFacing = (1, 0)
    ,cShape = color yellow $ polygon [(0,0), (0,30), (30,30), (30,0)]
    ,cSpeed = 1
    ,cBB = BB { centerX = fst (cLocation c1) + 15  -- Offset is now half of 30
              , centerY = snd (cLocation c1) + 15  -- Offset is now half of 30
              , halfWidth = 15                     -- Half-width matches half the new size
              , halfHeigth = 15                    -- Half-height matches half the new size
              , rotation = 90
              }
}

-- Scatter instance
scat:: Scatter
scat = Scatter {
    sName = "scatter"
    ,sLives = 1
    ,sLocation = (-100, 100) -- center of the thing 
    ,sDirection = (1,0)
    ,sFacing = (1, 0)
    ,sShape = color red $ polygon [(0,0), (0,60), (60,60),(60,0)]
    ,sSpeed = 1
    ,sBB = BB{ centerX = -70, centerY = 130, halfWidth = 30, halfHeigth = 30, rotation = 90}
    
}

-- UFO instance
uf:: UFO
uf = UFO{
    uLives = 1
    ,uFacing = (0,1)
    ,uLocation = (-100, 100) -- center of the thing 
    ,uDirection = (1,0)
    ,uShape = color green $ polygon [(0,0), (0,40), (40,40),(40,0)]
    ,uBB = BB{ centerX = -70, centerY = 130, halfWidth = 20, halfHeigth = 20, rotation = 90}
    ,invicible  = False
    ,aimDir = (-1,0)
    ,laserCld = 0
    
}

-- Direction the UFO will aim at
aimDirection :: Point -> Point -> Vector
aimDirection u p = normalize(fst aim, snd aim)
    where 
        aim = ((fst p )- (fst u), (snd p) - (snd u))

-- Checks if a UFO can shoot
ufoCanshoot :: UFO  -> Bool
ufoCanshoot u | laserCld  u < laserCooldown = False
              | otherwise = True

-- Filters the ufo if it can shoot
filterUfo :: UFO -> UFO
filterUfo u = if ufoCanshoot u then u{laserCld = 0} else u


