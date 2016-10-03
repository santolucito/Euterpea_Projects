{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

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
import Types

import Debug.Trace

mainSF :: SF (Event G.Event) Picture
mainSF = proc e ->
  do
    click <- arr (filterE (isJust. toYampaEvent)) -< e 
    r1 <- colControl (-100) -< click
    r2 <- colControl (-50) -< click
    r3 <- colControl 0 -< click
    r4 <- colControl 50 -< click
    r5 <- colControl 100 -< click
    finalPic <- arr drawGame -< concatMap (map (\b-> (myColor b,currPos b))) [r1,r2,r3,r4,r5]
    returnA  -< finalPic
  where
    drawGame slide = renderUI 10 slide
   

--build a column of buttons at an x pos
colControl :: Int -> SF (Event G.Event) [Block]
colControl x = proc e ->
  do
    rec
      b1' <- iBlock -< b1
      b2' <- iBlock -< b2
      b3' <- iBlock -< b3
      let bs = [b1',b2',b3']
      b1 <- buttonControl black x (x-40) -< ([b2',b3'],e)
      b2 <- buttonControl yellow x x -< ([b1',b3'],e)
      b3 <- buttonControl azure x (x+40) -< ([b1',b2'],e)
    returnA -< [b1,b2,b3]

--takes a list of positions of buttons in its row
--checks for collisions on the y and give updated position
buttonControl :: Color -> Int -> Int -> SF ([Block],Event G.Event) Block
buttonControl c x y = proc (ps,e) ->
  do
    rec 
      qState'  <- iPre Quantum -< qState
      qState   <- arr changeQState -< (isEvent e,qState',ps)
      
      direction' <- iPre 1 -< direction
      sliderV'   <- iPre y -< sliderV
      
      dirC       <- arr sliderDir -< (sliderV',direction')
      direction  <- arr checkCollision -< (dirC,sliderV',ps)
      sliderV    <- arr (sliderPos (x,y)) -< (sliderV',direction,qState,ps)
      
    returnA -< Block {myColor = c,
                      origPos = (x,y),
                      currPos = (x,sliderV)}
  where
    changeQState (e,q,bs) = if
      | e && q==Classic -> ReQuant
      | e && q==Quantum -> Classic
      | e && q==ReQuant -> Classic
      | q==ReQuant -> if inOrig bs then Quantum else ReQuant
      | otherwise -> q
    sliderDir (slide,dir) = if abs slide>150 then (-1*dir) else dir
    sliderPos orig (slide,dir,q,ps) =
      let
        bound b p = min b $ max (-b) p
	quantum = slide + dir*1
	requantize = if slide ==snd orig then slide else slide+1
	collapse = if nearBy (slide-1) ps then slide else slide -1
	new_p = if 
	  | q==Quantum -> quantum
	  | q==Classic -> collapse
	  | q==ReQuant -> requantize
      in
        bound 151 (trace (show q) new_p)
    nearBy y ys  = any (\y' -> abs (y-y') < 20) $ map (snd.currPos) ys
    checkCollision (dir,slide,ps) = if slide `nearBy` ps then (-1*dir) else dir

playGame :: IO ()
playGame =do
  do
    playYampa
        (InWindow "Yampa Example" (420, 360) (800, 600))
        white
        30
        mainSF

{-instance ArrowChoice SF where
  left = switch-}
