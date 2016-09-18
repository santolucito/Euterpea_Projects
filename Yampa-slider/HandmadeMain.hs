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
    r1 <- colControl (-100) -< ()
    r2 <- colControl (-50) -< ()
    r3 <- colControl 0 -< ()
    r4 <- colControl 50 -< ()
    r5 <- colControl 100 -< ()
    finalPic <- arr drawGame -< concat [r1,r2,r3,r4,r5]
    returnA  -< finalPic
  where
    drawGame slide = renderUI 10 slide
   

--build a column of buttons at an x pos
colControl :: Int -> SF () [(Color,(Int,Int))]
colControl x = proc _ ->
  do
    rec
      b1' <- iPre (0,0) -< b1
      b2' <- iPre (0,0) -< b2
      b3' <- iPre (0,0) -< b3
      let bs = [b1',b2',b3']
      b1 <- buttonControl x (x-40) -< [b2',b3']
      b2 <- buttonControl x x -< [b1',b3']
      b3 <- buttonControl x (x+40) -< [b1',b2']
    returnA -< [(black,b1),(yellow,b2),(azure,b3)]

--takes a list of positions of buttons in its row
--checks for collisions on the y and give updated position
buttonControl :: Int -> Int -> SF [(Int,Int)] (Int,Int)
buttonControl x y = proc ps ->
  do
    rec 
      direction' <- iPre 1 -< direction
      sliderV'   <- iPre y -< sliderV
      
      dirC       <- arr sliderDir -< (sliderV',direction')
      direction  <- arr checkCollision -< (dirC,sliderV',ps)
      sliderV    <- arr sliderPos -< (sliderV',direction)
    returnA  -< (x,sliderV)
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
