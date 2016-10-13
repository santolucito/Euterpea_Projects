--  LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GameLogic where

import System.Random (StdGen)
import Prelude hiding (Left,Right)
import Types 
-- import FRP.Yampa
import LevelMaps

import Control.Lens
import Codec.Picture hiding (imageData)
--import Graphics.Gloss.Juicy

import System.IO.Unsafe
import Debug.Trace

initialState :: StdGen -> GameState 
initialState g = GameState { 
   _board = emptyBoard
  ,_status = InProgress
  ,_gen = g
}

emptyBoard = Board {
 _player1 = Player {
   --imageSrc =  "pics/stand_east.png"
   _imageSrc =  "pics/east.gif"
  ,_position = (0,0)
  ,_dir      = Types.Left
  ,_score    = 0},
 _walls = smallRoom,
 _image = "pics/bkgd.png",
 _imageData = either whiteImage convertRGB8 $ unsafePerformIO $ readImage "pics/bkgd.png"
}

whitePixel = PixelRGB8 0 0 0
whiteImage = (\_-> generateImage (\_ _ -> whitePixel) 1 1)

isGameOver :: GameState -> Bool
isGameOver s = False

update :: (GameState, GameInput) -> GameState
update (gameState, input) =
    case input of
      None -> id gameState
      dir -> move dir gameState

move :: Direction -> GameState -> GameState
move d g = if collision (makeMove d g) then g else makeMove d g

collision :: GameState -> Bool
collision g = let
  c = pixelAtFromCenter (view (board.imageData) g) x y
  (x,y) = view (board.player1.position) g
 in (traceShow c c) == whitePixel

pixelAtFromCenter :: Pixel a => Image a -> Int -> Int -> a
pixelAtFromCenter i x y = let
  h = imageHeight i 
  w = imageWidth i 
 in
  pixelAt i (x+(w `div` 2)) (y+(h `div` 2))

makeMove :: Direction -> GameState -> GameState
makeMove d g = over (board.player1.position) (appT updateF) g
 where
  updateF = case d of
   Down  -> (0,-1)
   Up    -> (0,1)
   Left  -> (-1,0)
   Right -> (1,0)
   _     -> (0,0)
  appT (dx,dy) (x,y) = (x+dx,y+dy)
