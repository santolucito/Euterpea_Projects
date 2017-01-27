module Render.GlossInterface where

import Prelude hiding (id, (.))

import FRP.Yampa

import Graphics.Gloss (Color, Display, Picture(Blank))
import qualified Graphics.Gloss.Interface.IO.Game as G

import Data.IORef
import System.Mem

playYampa :: 
    Display ->
    Color ->
    Int ->
    SF (Event G.Event) Picture ->
    IO ()

playYampa display color frequency network = 
  do
    vPic <- newIORef Blank
    events <- newIORef NoEvent

    handle <- reactInit 
        (return NoEvent)
        (\_ changed pic -> 
          do
            if changed then vPic `atomicWriteIORef` pic else return ()
            return False)
        network
    
    _ <- react handle (infts, Just NoEvent)

    -- Since `react` requires nonzero time intervals,
    -- we pass infinitesimal time intervals and accumulate them on
    -- the variable `t`. Then every frame `delta` is corrected by `t`.
    G.playIO
        display
        color
        frequency
        infts -- initial t. This is for initial step
        (const $ readIORef vPic)
        -- does not handle multiple events within one update cycle
        -- TODO make events :: IORef [Event]?
        (\e t -> writeIORef events (Event e) >> return (t+infts))
        (\delta t ->
            let 
                delta' = realToFrac delta - t
              in
                if delta' > 0
                  then do
                    e <- readIORef events
                    _ <- react handle (delta', Just e)
                    atomicWriteIORef events NoEvent
                    return 0.0
                  else
                    return (-delta'))
  where
    infts = 0.01 / fromIntegral frequency
