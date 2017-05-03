{-#  LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Arrows #-}

module GameLogic where

import System.Random (StdGen)
import Prelude hiding (Left,Right)
import Types.Common
import Types.GameObjs
import Render.ImageIO
import InitGameState

import Control.Lens
import Codec.Picture 
import qualified Data.HashSet as S

import Settings

import FRP.Yampa

import Debug.Trace
update :: SF (GameState, GameInput) GameState
update = proc (gameState, input) -> 
  do
    t <- time -< ()
    gs <- arr (uncurry trackTime) -< (t, gameState)
    moved <- arr useInput -< (gs,input)
    collisons <- arr findObjCollisions -< moved 
    returnA -< collisons
  where
    --TODO make continuos time based motion
    useInput (gameState,input) = case input of
         None -> set (board.player1.inMotion) False gameState
         dir -> move dir gameState

trackTime :: Time -> GameState -> GameState
trackTime = set (board.player1.aliveTime) 

move :: Direction -> GameState -> GameState
move d g = if wallCollision (makeMove d g) then g else makeMove d g


findObjCollisions :: GameState -> GameState
findObjCollisions g =
  over (board.objs) (S.map (updateCollide g)) g

--what happens to an obj when the player collides with it
updateCollide :: GameState -> GameObj -> GameObj
updateCollide g o = 
  if elem (_position o) (playerLocs g)
  then set display False o
  else o

--what happens to the player when it collides with an obj
--playerCollideUpdate :: ??

didCollide :: GameState -> GameObj -> GameObj -> Bool
didCollide gs g1 g2 = undefined

wallCollision :: GameState -> Bool
wallCollision g = let
  boardPixels = map (\(x,y) -> pixelAtFromCenter (fst $ getImg _levelName g) x y) (playerLocs g)
 in 
  any (==blackAPixel) (boardPixels)

--TODO for pixel level detection
--rather than building rect, get positions of all nonalpha pixels
playerLocs :: GameState -> [(Int,Int)]
playerLocs g = let
  (x,y) = view (board.player1.gameObj.position) g
  objImg = fst $ getImg (_player1) g
  xsize = imageWidth objImg
  ysize = imageHeight objImg
 in
  [(x',y') | x' <- [x-xsize..x+xsize],y' <- [y-ysize.. y+ysize]]

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
    newPos = over (board.player1.gameObj.position) (appT updateF) g
    newDir = set (board.player1.dir) d newPos
  in
    set (board.player1.inMotion) True newDir

-- | conviences

isGameOver :: GameState -> Bool
isGameOver s = False

traceMe x = traceShow x x
