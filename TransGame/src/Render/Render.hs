
module Render.Render where

import Data.Monoid
import Graphics.Gloss

import Types.Types
import FRP.Yampa
import Render.ImageIO

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

-- | keep the player centered at all times
placePlayer :: GameState -> Picture
placePlayer g = let
   p = snd $ getImg player1 playerImgs g
   --(x,y) = mapTup fromIntegral ((view (board.player1.position)) g)
 in
   translate 0 0 p
     
-- | move the background around the player
placeBkgd :: GameState -> Picture
placeBkgd g = let
   bkgd = snd$ getImg levelName levelImgs g 
   (x,y) = mapTup fromIntegral $ (position.player1.board) g
 in
   translate (-x) (-y) bkgd

placeText :: GameState -> Picture
placeText g = 
   translate (50) (120) $ text $ show $ (10000 - (aliveTime.player1.board) g)

mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup f (a1, a2) = (f a1, f a2)
