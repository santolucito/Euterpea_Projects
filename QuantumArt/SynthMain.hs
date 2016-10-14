{-# LANGUAGE Arrows #-}
{-# LANGUAGE ScopedTypeVariables #-}

module
    SynthMain
where

import Prelude hiding (id, (.))
import Control.Arrow

import FRP.Yampa
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

import Buttons
import GlossInterface

import Mealy (stateTrans)

mainSF :: SF (Event G.Event) Picture
mainSF = proc c ->
 do
   rec
     --the state of the mealy machine (one big rec)
     d' <- iPre 0 -< d 
     p' <- iPre 0 -< p
     s' <- iPre 0 -< s
     v' <- iPre 0 -< v
     --c' -< iPre 0 <- c (click is in input stream)
     --the updates (one big arr)
     (p,v,s,d) <- arr stateTrans -< (p',v',c,s',d')
   returnA -< renderUI s p 
  
playGame :: IO ()
playGame =
  do
    playYampa
        (InWindow "Yampa Example" (320, 240) (800, 600))
        white
        30
        mainSF
