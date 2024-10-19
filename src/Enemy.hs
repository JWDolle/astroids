-- Enemy specific code goes here

module Enemy where

import Entity


data Comet = Scatter { --become small comets
    sName:: String
    ,sLives :: Int
    ,sLocation :: Point
    ,sDirection :: Vector
    ,sSHape:: Picture
}

data Comet = UFO { --shoots the player
    uName :: String
    ,uLives :: Int
    ,uLocation :: Point
    ,uDirection :: Vector
}
data Comet = Comet { -- no intelligence
     c:: Int
    ,cLives :: Int
    ,cLocation :: Point
    ,cDirection :: Vector
    ,
}