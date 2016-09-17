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
    b1 <- buttonControl (-99) -< ()
    b2 <- buttonControl (-50) -< ()
    b3 <- buttonControl 0 -< ()
    b4 <- buttonControl 50 -< ()
    b5 <- buttonControl 99 -< ()
    finalPic <- arr drawGame -< [b1,b2,b3,b4,b5]
    returnA  -< finalPic
  where
    sliderDir (slide,dir) = if abs slide>100 then (-1*dir) else dir
    sliderPos (slide,dir) = slide + dir*1
    drawGame slide = renderUI 10 slide
   

buttonControl :: Int -> SF () Int
buttonControl i = proc _ ->
  do
    rec 
      direction' <- iPre 1 -< direction
      sliderV'   <- iPre i -< sliderV
      
      direction  <- arr sliderDir -< (sliderV',direction')
      sliderV    <- arr sliderPos -< (sliderV',direction)
    returnA  -< sliderV
  where
    sliderDir (slide,dir) = if abs slide>100 then (-1*dir) else dir
    sliderPos (slide,dir) = slide + dir*1

playGame :: IO ()
playGame =
  do
    playYampa
        (InWindow "Yampa Example" (320, 240) (800, 600))
        white
        30
        mainSF
