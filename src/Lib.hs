module Lib
( Coordinate(..)
, Direction(..)
, Snake(..)
, World(..)
, dUp
, dDown
, dLeft
, dRight
, initialWorld
, inBox
, extend
, move
, changeDirection
, collides
, collidesApple 
, applePicture
, snakePicture
, step
, nextWorld
, worldPicture
) where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture 
import System.Random

type Coordinate  = (Int,Int)
type Direction   = (Int,Int)
type Apple       = (Coordinate,StdGen)      

data Snake = Snake { body :: [Coordinate]
                   , sdir :: Direction } deriving (Eq, Show)

data Status = Alive | Dead
    deriving (Show, Eq)

data World = World { snake :: Snake 
                   , time :: Float 
                   , status :: Status 
                   , apple :: Apple } deriving (Show)

width = 64
height = 64
speed = 0.2

dUp :: Direction
dUp = (0,-1)

dDown :: Direction
dDown = (0,1)

dLeft :: Direction
dLeft = (-1,0)

dRight :: Direction
dRight = (1,0)

initialPosition :: Snake
initialPosition = Snake [(10,10), (10,9)] dUp

initialWorld = World initialPosition 0 Alive ((10,1), mkStdGen 12345)

snake_scale  :: Int
snake_scale  = 10  

inBox :: Coordinate -> Bool
inBox (x,y) = (x >= 0) && (y >= 0) && (x < 64) && (y < 64)

extend :: [Coordinate] -> Direction -> [Coordinate]
extend l@((x,y):xs) (dx,dy) = (x + dx, y + dy) : l

move :: Snake -> Snake
move (Snake s d) = Snake (init $ extend s d) d

changeDirection :: Direction ->  World -> World
changeDirection new_d (World (Snake s d) t st a) = World (Snake s new_d) t st a

collides :: [Coordinate] -> Coordinate -> Bool                                                                                          
collides l p =  (filter (\x -> x == p) l) /= []

nextApple :: Apple -> Apple
nextApple (_, r) = ((a,b), r2)
  where
    (a,r1) = randomR (0, width - 1) r
    (b,r2) = randomR (0, height - 1) r1

collidesApple :: Snake -> Apple -> Apple                                     
collidesApple (Snake s _) a@(c, _) | (head s) == c = nextApple a
                                   | otherwise = a

nextWorld :: World -> World
nextWorld w@(World s@(Snake b d) t st a@(cA,r)) | not $ inBox (head b) = (World initialPosition 0.0 Dead (nextApple a))
                                                | collides (tail b) (head b) = (World initialPosition 0.0 Dead (nextApple a))
                                                | cA == (head b) = (World ( Snake (extend b d) d) t st(nextApple a))
                                                | otherwise = w

step :: Float -> World -> World
step dt (World s t st a) | (dt + t) < speed = (World s (dt + t) st a)
                         | otherwise = (World (move s) 0.0 st a)

cartToScreen :: Coordinate -> Coordinate
cartToScreen (x,y) = (x + width `div` 2, height `div` 2 - y)

screenToCart :: Coordinate -> Coordinate
screenToCart (x,y) = (x  - width `div` 2,  (-y) + height `div` 2)

applePicture :: Apple -> Picture 
applePicture (c, _) = translate xC yC $ pictures [Color black $ rectangleSolid 10 10, Color red $ rectangleSolid 8 8] 
    where (xS, yS) = screenToCart c
          xC = fromIntegral (xS * 10) 
          yC = fromIntegral (yS * 10)

snakePicture :: Snake -> Picture
snakePicture s = pictures $ snakePicture' s

snakePicture' :: Snake -> [Picture]
snakePicture' (Snake [] _) = []
snakePicture' (Snake ((x,y):xs) d) = sp : snakePicture' (Snake xs d)
    where (xS, yS) = screenToCart (x,y)
          xC = fromIntegral (xS * 10) 
          yC = fromIntegral (yS * 10)
          sp = translate xC yC $ Color black $ rectangleSolid 10 10 

worldPicture :: World -> Picture
worldPicture (World s _ _ a) = pictures [snakePicture s, applePicture a]
