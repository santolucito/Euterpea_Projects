{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}

module
    HandmadeMain
where

import Prelude hiding (id, (.))
import Control.Category
import Control.Arrow
import Data.Functor ((<$))
import Data.Maybe (isJust)

import FRP.Yampa
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

import Buttons
import GlossInterface


mainSF :: SF (Event G.Event) Picture
mainSF = proc e ->
  do
    click <- arr (filterE (isJust. toYampaEvent)) -< e 
    r1 <- colControl (-100) -< click
    r2 <- colControl (-50) -< click
    r3 <- colControl 0 -< click
    r4 <- colControl 50 -< click
    r5 <- colControl 100 -< click
    finalPic <- arr drawGame -< concat [r1,r2,r3,r4,r5]
    returnA  -< finalPic
  where
    drawGame slide = renderUI 10 slide
   

--build a column of buttons at an x pos
colControl :: Int -> SF (Event G.Event) [(Color,(Int,Int))]
colControl x = proc e ->
  do
    rec
      b1' <- iPre (0,0) -< b1
      b2' <- iPre (0,0) -< b2
      b3' <- iPre (0,0) -< b3
      let bs = [b1',b2',b3']
      b1 <- buttonControl x (x-40) -< ([b2',b3'],e)
      b2 <- buttonControl x x -< ([b1',b3'],e)
      b3 <- buttonControl x (x+40) -< ([b1',b2'],e)
    returnA -< [(black,b1),(yellow,b2),(azure,b3)]

--takes a list of positions of buttons in its row
--checks for collisions on the y and give updated position
buttonControl :: Int -> Int -> SF ([(Int,Int)],Event G.Event) (Int,Int)
buttonControl x y = proc (ps,e) ->
  do
    rec 
      direction' <- iPre 1 -< direction
      sliderV'   <- iPre y -< sliderV
      
      dirC       <- arr sliderDir -< (sliderV',direction')
      direction  <- arr checkCollision -< (dirC,sliderV',ps)
      sliderV    <- arr sliderPos -< (sliderV',direction)
    returnA -< (x,if isEvent e then sliderV'-1 else sliderV)
  where
    sliderDir (slide,dir) = if abs slide>150 then (-1*dir) else dir
    sliderPos (slide,dir) = slide + dir*1
    nearBy y ys  = any (\y' -> abs (y-y') < 20) ys
    checkCollision (dir,slide,ps) = if slide `nearBy` (map snd ps) then (-1*dir) else dir

playGame :: IO ()
playGame =
  do
    playYampa
        (InWindow "Yampa Example" (420, 360) (800, 600))
        white
        30
        mainSF

{-instance ArrowChoice SF where
  left = switch-}
