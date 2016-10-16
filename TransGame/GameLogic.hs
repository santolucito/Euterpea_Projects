--  LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GameLogic where

import System.Random (StdGen)
import Prelude hiding (Left,Right)
import Types 
import ImageIO

import Control.Lens
import Codec.Picture 

import Debug.Trace

initialState :: StdGen -> Images -> GameState 
initialState g is = GameState { 
   _board = emptyBoard is
  ,_status = InProgress
  ,_gen = g
  ,_images = is
}

emptyBoard is = Board {
 _player1 = Player {
   _position = (0,0)
  ,_dir      = Types.Left
  ,_score    = 0},
 _levelName = "mazeCircle"
}


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
  (x,y) = view (board.player1.position) g
  c = pixelAtFromCenter (getLevelImg g) x y
 in c == blackAPixel

pixelAtFromCenter :: Image PixelRGBA8 -> Int -> Int -> PixelRGBA8
pixelAtFromCenter i x y = let
  h = imageHeight i 
  w = imageWidth i 
  x' = (w `div` 2) + x
  y' = (h `div` 2) + (-y)
 in
  pixelAt i x' y'
  --whitePixel

makeMove :: Direction -> GameState -> GameState
makeMove d g = let 
    updateF = case d of
     Down  -> (0,-1)
     Up    -> (0,1)
     Left  -> (-1,0)
     Right -> (1,0)
     _     -> (0,0)
    appT (dx,dy) (x,y) = (x+dx,y+dy)
    newPos = over (board.player1.position) (appT updateF) g
    newDir = set (board.player1.dir) d newPos
  in
    newDir