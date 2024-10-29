-- Enemy specific code goes here
{-# LANGUAGE RecordWildCards #-}


module Enemy where
import Graphics.Gloss
import Entity
import Constants
import BoundingBox
import Random


---Data types
cometWidth :: Float
cometWidth = 30
cometHeigth :: Float
cometHeigth = 30

scatterWidth :: Float
scatterWidth = 60
scatterHeigth:: Float
scatterHeigth = 60

    
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
    uName :: String
    ,uLives :: Int
    ,uLocation :: Point
    ,uDirection :: Vector
    ,uShape :: Picture
    ,uBB :: BoundingBox

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
 

-- spawnEnemy :: Enemy -> GameState -> GameState
-- spawnEnemy (Comet n l loc dir f s sp r bb) gState@(GameState i e p c u s l b r Playing) = (GameState i e p (newCom:c) u s l b (snd randomDirectionY) Playing)
--     where
--         randomLocationX = randomRange (0,screenSize) r
--         randomLocationY = randomRange (0,screenSize) (snd randomLocationX)

--         randomDirectionX = randomRange (0,2) (snd randomLocationY)
--         randomDirectionY = randomRange (0,2) (snd randomDirectionX)

--         newCom = (Comet n l ((fst randomLocationX) - ((fromIntegral screenSize) / 2), (fst randomLocationY) - ((fromIntegral screenSize) / 2)) ((fst randomDirectionX) - 1, (fst randomDirectionY) - 1) f s sp r bb)



----EXAMPLE ENIMIES

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

scat:: Scatter
scat = Scatter {
    sName = "Test comet"
    ,sLives = 1
    ,sLocation = (-100, 100) -- center of the thing 
    ,sDirection = (1,0)
    ,sFacing = (1, 0)
    ,sShape = color red $ polygon [(0,0), (0,60), (60,60),(60,0)]
    ,sSpeed = 1
    ,sBB = BB{ centerX = -70, centerY = 130, halfWidth = 30, halfHeigth = 30, rotation = 90}
    
}

uf:: UFO
uf = UFO{
    uName = "Test comet"
    ,uLives = 1
    ,uLocation = (-100, 100) -- center of the thing 
    ,uDirection = (1,0)
    ,uShape = color blue $ polygon [(0,0), (0,60), (60,60),(60,0)]
    ,uBB = BB{ centerX = -70, centerY = 130, halfWidth = 30, halfHeigth = 30, rotation = 90}
}



