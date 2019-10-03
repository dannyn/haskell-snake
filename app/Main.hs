module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Lib

drawPicture :: World -> Picture
drawPicture w = worldPicture w 

updateModel :: Float -> World -> World
updateModel t w = nextWorld $ step t w

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey KeyUp) Down _ _) w = changeDirection dUp w
inputHandler (EventKey (SpecialKey KeyDown) Down _ _) w =  changeDirection dDown w
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) w = changeDirection dRight w 
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) w = changeDirection dLeft w
inputHandler _ w = w

main :: IO ()
main = play
    (InWindow "Circle Window" (640, 640) (10, 10))
    white
    60
    initialWorld
    drawPicture
    inputHandler
    updateModel
