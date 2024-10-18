{-# LANGUAGE RecordWildCards #-}
-- Entity code goes here (shared code for player, enemies and projectiles)
module Entity where


import Graphics.Gloss

pVelocity :: Float
pVelocity = 0.1;

data Moving = STOP | MOVING

data Player = Player {
                  pName :: String
                , pLives :: Int 
                , pLocation :: Point
                , pDirection :: Vector
                , pShape :: Picture
                , pSpeed :: Float

                }
p1 :: Player
p1 = Player {
                pName = "player 1"
                , pLives = 3
                , pLocation = (0,0)
                , pDirection = (0,1)
                , pShape = color blue $ translate (-200) 0 $ scale 10 10 $ polygon[(0,0), (3,0), (1.5,3)]
                , pSpeed = pVelocity            
            }


data Comet = Comet {
    cNum :: Int
    ,cLives :: Int
    ,cLocation :: Point
    ,cDirection :: Vector
}



class Moveable a where
    move:: a -> a
--rotate::  a -> a


-- moveable objects are things that can rotate and move 
instance Moveable Player where
    move p@Player {..} = Player pName pLives adjusted pDirection shap pSpeed -- might have to change this later 
        where 
            adjusted = (fst pLocation + fst pDirection * pSpeed, snd pLocation + snd pDirection * pSpeed)
            shap = translate (fst adjusted) (snd adjusted) pShape
--rotate:: Player -> Player
--rotate v p@(Player{..}) = Player pName pLives pLocation adjusted shap pSpeed 


            
