

module Sprites where
import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicy)

data Sprites = Sprites{  
    sPlayer :: Picture
   , sComet :: Picture
   , sScatter :: Picture
   , sUfo :: Picture
}


loadSprites :: IO Sprites
loadSprites = do
    player  <- loadJuicy "Sprites/SpaceShip.png"
    comet   <- loadJuicy "Sprites/Comet.png"
    scatter <- loadJuicy "Sprites/Scatter.png"
    ufo     <- loadJuicy "Sprites/UFO.png"
    
    -- Handle loading failures
    return $ Sprites
        (maybe Blank id player)
        (maybe Blank id comet)
        (maybe Blank id scatter)
        (maybe Blank id ufo)