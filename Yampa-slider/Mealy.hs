{-# LANGUAGE MultiWayIf#-}

module Mealy where

import Data.Maybe (isJust)

import FRP.Yampa
import qualified Graphics.Gloss.Interface.IO.Game as G

import Buttons

leftmost p    = (p>90)
rightmost p   = (p<(-90))
centermost p  = (p>(-10) && p <10)
isClick       = isEvent. filterE (isJust. toYampaEvent)
moveLeft      = (-)
moveRight     = (+)
updateVel     = id
updateScore   = id


stateTrans :: (Int,Int,Event G.Event,Int,Int) -> (Int,Int,Int,Int)
stateTrans (p,v,c,s,d) =
  if 
    | p1(p,v,c,s,d) -> c1 (p,v,c,s,d)
    -- | p2(p,v,c,s,d) -> c2 (p,v,c,s,d)

p1 (p,v,c,s,d) = 
  rightmost p 
  && (not.centermost) p 
  && (not. isClick) c 
  && (not. leftmost) p 
  && (d==0)
c1 (p,v,c,s,d) = (p',v',s',d')
  where 
    p' = moveLeft p v
    v' = updateVel 1
    s' = updateScore 0
    d' = id d

