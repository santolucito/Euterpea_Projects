module Buttons where

import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.Pure.Game
import Text.Printf
import Control.Applicative

buttonC0 :: Extent 
buttonC0  = makeExtent   (-200)   (-230)  25 (-25)
rows :: [Extent]
rows = repeat (makeExtent (-5) (-35) 0 (-50))

-- Button rendering

renderUI :: Int 
         -> [(Color,(Int,Int))]
         -> Picture
renderUI count0 locs = 
  let
    s = zip rows locs 
    ss = mconcat $ map (\(widget,(c,(x,y))) -> translate (fromIntegral x) (fromIntegral y) (renderSlider widget c (show y))) s
  in
   renderButton buttonC0 white "Motion" <> 
   ss
    
unify :: Int -> Maybe Int -> String
unify n Nothing               = show n
unify n (Just n') | n == n'   = show n
                  | otherwise = printf "%d != %d" n n'


renderSlider:: Extent -> Color -> String -> Picture
renderSlider ex c s = color c bg -- <> color white fg
  where
    --bg = polygon (cornerPoints ex)
    --bg = circle (20)
    bg = thickCircle 10 (20) --cornerPoints ex)

renderButton :: Extent -> Color -> String -> Picture
renderButton ex c s = color c bg  <> color black fg
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

