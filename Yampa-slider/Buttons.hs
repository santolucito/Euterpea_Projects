module Buttons where

import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.Pure.Game
import Text.Printf
import Control.Applicative

buttonC0 :: Extent 
buttonC0  = makeExtent   35     5  (125) (75)
rows' :: [(Int, Int) -> Extent]
rows' = map (\x (low, high) -> makeExtent low high x (x-50)) [120,60,0,-60,-120]

rows :: [Extent]
rows = map (\x -> makeExtent (-5) (-35) x (x-50)) [120,60,0,-60,-120]

discs :: [Extent]
discs = zipWith ($) rows' (replicate 5 (-5,-35))
--why isnt this the same as above?
--discs = rows' <*> (replicate 5 (-5,-35))

-- Button rendering

renderUI :: Int 
         -> [Int]
         -> Picture
renderUI count0 locs = 
  let
    s = zip discs locs
    ss = mconcat $ map (\(widget,pos) -> translate 0 (fromIntegral pos) (renderButton widget (show pos))) s
  in
   --renderButton buttonC0 (show count0) <> 
   ss
    
unify :: Int -> Maybe Int -> String
unify n Nothing               = show n
unify n (Just n') | n == n'   = show n
                  | otherwise = printf "%d != %d" n n'


renderButton :: Extent -> String -> Picture
renderButton ex s = color azure bg <> color white fg
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

isClickedBy :: Extent -> Event -> Bool
isClickedBy ex (EventKey (MouseButton LeftButton) Down _ p) = pointInExtent ex p
isClickedBy _ _ = False

toYampaEvent :: Event -> Maybe ButtonClick
toYampaEvent e | buttonC0 `isClickedBy` e = Just Click
               | otherwise                = Nothing

