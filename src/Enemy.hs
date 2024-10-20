-- Enemy specific code goes here
{-# LANGUAGE RecordWildCards #-}


module Enemy where
import Graphics.Gloss
import Entity
import Constants


--Constanst regarding enemies
c_rAngle :: Float
c_rAngle = 5

---Data types

data Enemy = S Scatter | U UFO | C Comet

data Scatter = Scatter { --become small comets
    sName:: String
    ,sLives :: Lives
    ,sLocation :: Point
    ,sDirection :: Vector
    ,sSHape:: Picture
}

data UFO = UFO { --shoots the player
    uName :: String
    ,uLives :: Lives
    ,uLocation :: Point
    ,uDirection :: Vector
    ,uShape :: Picture
}
data Comet = Comet { -- no intelligence
    cName:: String

    ,cLives :: Lives
    ,cLocation :: Point
    ,cDirection :: Vector
    ,cFacing :: Vector
    ,cShape :: Picture
    ,cSpeed :: Float
    ,cRotateL :: Bool
}


instance Moveable Enemy where
    move (C c@Comet {..}) = C c { 
        cLocation = adjusted
    }
        where 
            adjusted = (fst cLocation + fst cDirection * cSpeed,
                        snd cLocation + snd cDirection * cSpeed)

    move (U u@UFO {..}) = U u {
        uLocation = adjusted
    }
        where
            adjusted = (fst uLocation + fst uDirection * 2,
                        snd uLocation + snd uDirection * 2)

    move (S s@Scatter {..}) = S s {
        sLocation = adjusted
    }
        where
            adjusted = (fst sLocation + fst sDirection * 1.5,
                        snd sLocation + snd sDirection * 1.5)

    rotate_ (C c@Comet {..}) = C c {
        cFacing = adjusted
    }
        where
            angleRadians = radians c_rAngle
            cAngle = extractAngle cFacing
            nAngle = if cRotateL then cAngle - angleRadians else cAngle + angleRadians
            adjusted = (cos nAngle, sin nAngle)

    rotate_ (U u@UFO {..}) = U u { 
        uDirection = adjusted
    }
        where
            angleRadians = radians 10
            uAngle = extractAngle uDirection
            nAngle = uAngle + angleRadians
            adjusted = (cos nAngle, sin nAngle)

    rotate_ (S s@Scatter {..}) = S s { 
        sDirection = adjusted
    }
        where
            angleRadians = radians 5
            sAngle = extractAngle sDirection
            nAngle = sAngle + angleRadians
            adjusted = (cos nAngle, sin nAngle)
    

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