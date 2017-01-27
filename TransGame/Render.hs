
module Render where

import Data.Monoid
import Graphics.Gloss

import Graphics.Gloss.Juicy

import Data.Maybe
import System.IO.Unsafe

import Types
import FRP.Yampa
import ImageIO

import Control.Lens

-- | After parsing the game input and reacting to it we need to draw the
-- current game state which might have been updated
drawGame :: SF GameState Picture
drawGame = arr renderState

renderState :: GameState -> Picture
renderState s = 
  placeBkgd s <>
  placePlayer s <>
  placeText s

placeBkgd :: GameState -> Picture
placeBkgd g = let
   bkgd = fromJust$ unsafePerformIO $ loadJuicy $ "pics/mazeCircleBig.png"
   p = view (board.player1) g
   (x,y) = mapTup fromIntegral (view position p)
 in
   translate (-x) (-y) bkgd

placePlayer :: GameState -> Picture
placePlayer g = let
   p = getPlayerPic (view (images.playerImgs) g) (view (board.player1) g)
   (x,y) = mapTup fromIntegral ((view (board.player1.position)) g)
 in
   translate 0 0 p
     
placeText :: GameState -> Picture
placeText g = 
   translate (50) (120) $ text $ show $ (10000 - view (board.player1.aliveTime) g)

mapTup f (a1, a2) = (f a1, f a2)
