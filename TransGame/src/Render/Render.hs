
module Render.Render where

import Data.Monoid
import qualified Data.Set as S
import Graphics.Gloss

import Types.Common
import Types.GameObjs
import FRP.Yampa
import Render.ImageIO

import Control.Lens (view)

-- | After parsing the game input and reacting to it we need to draw the
-- current game state which might have been updated
drawGame :: SF GameState Picture
drawGame = arr renderState

renderState :: GameState -> Picture
renderState s = 
  placeBkgd s <>
  placePlayer s <>
  --placeGameObjs s <>
  placeText s

placeGameObjs :: GameState -> [Picture]
placeGameObjs g = let
   os = view (board.objs) g :: S.Set GameObj
   f o = undefined -- translate (-fromIntegral$fst$_position o) (-fromIntegral$snd$_position o) $ snd $ getImg _img _playerImgs g
 in
   map f (S.toList os)
-- | keep the player centered at all times
placePlayer :: GameState -> Picture
placePlayer g = let
   p = snd $ getImg _player1 g
   --(x,y) = mapTup fromIntegral ((view (board.player1.position)) g)
 in
   translate 0 0 p
     
-- | move the background around the player
placeBkgd :: GameState -> Picture
placeBkgd g = let
   bkgd = snd$ getImg _levelName g 
   (x,y) = mapTup fromIntegral $ view (board.player1.gameObj.position) g
 in
   translate (-x) (-y) bkgd

placeText :: GameState -> Picture
placeText g = 
   translate (50) (120) $ text $ show $ (10000 - (_aliveTime._player1._board) g)

mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup f (a1, a2) = (f a1, f a2)
