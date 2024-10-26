module Button where
import Graphics.Gloss
import BoundingBox

data Button = Button {iShape :: Picture, iLocation :: Point, iDirection :: Vector, iBB :: BoundingBox} | NoButton



playButtonPic :: Picture
playButtonPic = polygon [(0,0),(0,100),(100,100), (100,0)]
playButtonBB :: BoundingBox
playButtonBB = BB{centerX = 0, centerY = 0, halfWidth = 50, halfHeigth = 50, rotation = 90}
playButtonOffset:: Point
playButtonOffset = (-50, 50)
playButtonIsFacing :: Vector
playButtonIsFacing = (0,1)
playButton :: Button
playButton = Button {iShape = color red playButtonPic, 
                     iLocation = playButtonOffset, 
                     iDirection = playButtonIsFacing,
                     iBB = playButtonBB}
