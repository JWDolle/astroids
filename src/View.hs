-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Entity
import Model


view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = pShape player
    where player = (fst (infoPlayer gstate))
