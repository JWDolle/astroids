module Main where

import Controller
import Model
import Player
import View
import Constants

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (screenSize, screenSize) (0, 0)) -- Or FullScreen
              black           -- Background color
              frameRate             -- Frames per second
              initialState    -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function