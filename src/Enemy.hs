-- Enemy specific code goes here
{-# LANGUAGE RecordWildCards #-}


module Enemy where
import Graphics.Gloss
import Entity


--Constanst regarding enemies
c_rAngle :: Float
c_rAngle = 5

---Data types

data Enemy = S Scatter | U UFO | C Comet

data Scatter = Scatter { --become small comets
    sName:: String
    ,sLives :: Int
    ,sLocation :: Point
    ,sDirection :: Vector
    ,sSHape:: Picture
}

data UFO = UFO { --shoots the player
    uName :: String
    ,uLives :: Int
    ,uLocation :: Point
    ,uDirection :: Vector
    ,uShape :: Picture
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
}

instance Moveable Enemy where
    move e@Comet {..} = e { 
        cLocation = adjusted
    }
        where 
            adjusted = (fst cLocation + fst cDirection * cSpeed,
                        snd cLocation + snd cDirection * cSpeed)
    
    move e@UFO {..} = e {
        uLocation = adjusted
    }
        where
            adjusted = (fst uLocation + fst uDirection * 2,  -- UFO-specific speed
                        snd uLocation + snd uDirection * 2)

    move e@Scatter {..} = e {
        sLocation = adjusted
    }
        where
            adjusted = (fst sLocation + fst sDirection * 1.5,  -- Scatter-specific speed
                        snd sLocation + snd sDirection * 1.5)

    -- Define rotation behavior for each enemy type
    rotate_ e@Comet {..} = e {
        cFacing = adjusted
    }
        where
            angleRadians = radians c_rAngle  -- Defined in Constants
            cAngle = extractAngle cFacing
            nAngle = if cRotateL then cAngle - angleRadians else cAngle + angleRadians
            adjusted = (cos nAngle, sin nAngle)
    
    rotate_ e@UFO {..} = e { -- UFOs may not rotate in your game, but if they do:
        uDirection = adjusted
    }
        where
            angleRadians = radians 10  -- Example: UFO-specific rotation speed
            uAngle = extractAngle uDirection
            nAngle = uAngle + angleRadians  -- UFO rotates in one direction
            adjusted = (cos nAngle, sin nAngle)
    
    rotate_ e@Scatter {..} = e { -- Scatter-specific rotation
        sDirection = adjusted
    }
        where
            angleRadians = radians 5  -- Scatter-specific rotation speed
            sAngle = extractAngle sDirection
            nAngle = sAngle + angleRadians  -- Scatter rotates in one direction
            adjusted = (cos nAngle, sin nAngle)


updateEnemies :: [Enemy] -> [Enemy]
updateEnemies = map update 

updateEnemy :: Enemy -> Enemy
updateEnemy e@Comet {..} = rotate_ $ move e
updateEnemy e@Scatter {..} = rotate_ $ move e
updateEnemy e@UFO {..} = move e



----EXAMPLE ENIMIES

c1 :: Comet
c1 = Comet {
    cName = "Test comet"
    ,cLives = 5
    ,cLocation = (-100, 100)
    ,cDirection = (1,0)
    ,cFacing = (1, 0)
    ,cShape = color red $ polygon [(0,0), (0,60), (60,60),(60,0)]
    ,cSpeed = 5
    ,cRotateL = True
}