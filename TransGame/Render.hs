
module Render where

import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.Pure.Game as G
import Text.Printf
import Control.Applicative

import Graphics.Gloss.Juicy

import Data.Maybe
import System.IO.Unsafe

import Types
import FRP.Yampa

import Control.Lens
import Control.Arrow

-- | After parsing the game input and reacting to it we need to draw the
-- current game state which might have been updated
drawGame :: SF GameState Picture
drawGame = arr renderState

renderState :: GameState -> Picture
renderState s = 
  placeBkgd s <>
  getPlayerPic s

placeBkgd :: GameState -> Picture
placeBkgd g = let
   --bkgd = fromImageRGB8 (view (board.imageData) g) 
   bkgd = fromJust$ unsafePerformIO $ loadJuicy $ "pics/bkgd.png"
   p = view (board.player1) g
   (x,y) = mapTup fromIntegral (view position p)
 in
   translate (-x) (-y) bkgd

getPlayerPic g = let
   p = fromJust$ unsafePerformIO $ loadJuicy $ (view (board.player1.imageSrc) g)
   (x,y) = mapTup fromIntegral ((view (board.player1.position)) g)
 in
   translate x y p
     
mapTup f (a1, a2) = (f a1, f a2)
