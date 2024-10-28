{-# LANGUAGE RecordWildCards #-}
-- Entity code goes here (shared code for player, enemies and projectiles)
module Entity where


import Graphics.Gloss
import Constants

pVelocity :: Float
pVelocity = 2;


class Moveable a where
    move:: a -> a
    rotate_::  a -> a



            
