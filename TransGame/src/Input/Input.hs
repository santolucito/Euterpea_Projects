{-# LANGUAGE Arrows #-}

module Input.Input where

import qualified Types.Common as T

import qualified Graphics.Gloss.Interface.IO.Game as G
import FRP.Yampa (Event(..), SF, arr, returnA, dHold, iPre)

import System.IO.Unsafe
import System.Exit

-- | Our game uses up, down, left and right arrows to make the moves, so
-- the first thing we want to do is to parse the Gloss Event into something
-- we are happy to work with (Direction data type)
parseInput :: SF (Event T.InputEvent) T.GameInput
parseInput = proc e -> do 
  keys <- dHold (G.EventResize (0,0)) -< e
  rec
    dir  <- arr readKey -< (keys ,oldDir)
    oldDir <- iPre T.None -< dir
  returnA -< dir
 where
  readKey (event,oldDir) = case event of 
    (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _) -> T.Up
    (G.EventKey (G.SpecialKey G.KeyDown) G.Down _ _) -> T.Down
    (G.EventKey (G.SpecialKey G.KeyLeft) G.Down _ _) -> T.Left
    (G.EventKey (G.SpecialKey G.KeyRight) G.Down _ _) -> T.Right
    (G.EventKey (G.SpecialKey G.KeySpace) G.Down _ _) -> ($!) (\_ -> T.Up) (unsafePerformIO exitSuccess) --Just for debugging
    (G.EventKey k G.Up _ _ ) -> turnOff k oldDir
    _ -> T.None
  turnOff k oldDir = if
     k == G.SpecialKey G.KeyUp && oldDir == T.Up ||
     k == G.SpecialKey G.KeyDown && oldDir == T.Down ||
     k == G.SpecialKey G.KeyLeft && oldDir == T.Left ||
     k == G.SpecialKey G.KeyRight && oldDir == T.Right 
     then T.None
     else oldDir

