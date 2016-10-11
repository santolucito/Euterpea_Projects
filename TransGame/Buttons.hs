{-# LANGUAGE RecordWildCards #-}

module Buttons where

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

buttonC0 :: Extent 
buttonC0  = makeExtent   35     5  200 (150)
rows :: [Extent]
rows = repeat (makeExtent (-5) (-35) 0 (-50))

bkgd :: Picture
bkgd = fromJust $ unsafePerformIO $ loadJuicy "pics/bkgd.png"


-- | After parsing the game input and reacting to it we need to draw the
-- current game state which might have been updated
drawGame :: SF GameState Picture
drawGame = arr renderState

renderState :: GameState -> Picture
renderState s = 
  bkgd <>
  getPlayerPic s

getPlayerPic GameState{..} =
 fromJust$ unsafePerformIO $ loadJuicy $ imageSrc $ player1 board  

-- Button rendering

renderUI :: Int 
         -> [(Color,(Int,Int))]
         -> Picture
renderUI count0 locs = 
  let
    s = zip rows locs 
    ss = mconcat $ map (\(widget,(c,(x,y))) -> translate (fromIntegral x) (fromIntegral y) (renderButton widget c (show y))) s
  in
   bkgd <>
   renderButton buttonC0 azure "Motion" <> 
   ss
    
unify :: Int -> Maybe Int -> String
unify n Nothing               = show n
unify n (Just n') | n == n'   = show n
                  | otherwise = printf "%d != %d" n n'


renderButton :: Extent -> Color -> String -> Picture
renderButton ex c s = color c bg <> color white fg
  where
    bg = polygon (cornerPoints ex)
    fg = translate x y
       $ uscale 0.1
       $ translate (-180) (-50)  -- vertically centered, random x offset :(
       $ text s
    (x, y) = coord2point (centerCoordOfExtent ex)


uscale :: Float -> Picture -> Picture
uscale v = scale v v


coord2point :: Coord -> Point
coord2point (x,y) = (fromIntegral x, fromIntegral y)

cornerCoords :: Extent -> [Coord]
cornerCoords ex = [(w,n), (e,n), (e,s), (w,s)]
  where
    (n, s, e, w) = takeExtent ex

cornerPoints :: Extent -> [Point]
cornerPoints = map coord2point . cornerCoords


-- Button events

data ButtonClick = Click | Toggle deriving (Show, Eq)

isClickedBy :: Extent -> G.Event -> Bool
isClickedBy ex (G.EventKey (G.MouseButton G.LeftButton) G.Down _ p) = pointInExtent ex p
isClickedBy _ _ = False

toYampaEvent :: G.Event -> Maybe ButtonClick
toYampaEvent e | buttonC0 `isClickedBy` e = Just Click
               | otherwise                = Nothing

