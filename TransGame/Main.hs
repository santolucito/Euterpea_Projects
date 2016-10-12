{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}

module Main where

import HandmadeMain
import Types
import Buttons
import GlossInterface

import FRP.Yampa (Event(..), SF, arr, tag, (>>>), returnA, dHold)
import qualified Graphics.Gloss.Interface.IO.Game as G

import System.Random (newStdGen, StdGen)

-- | Our game uses up, down, left and right arrows to make the moves, so
-- the first thing we want to do is to parse the Gloss Event into something
-- we are happy to work with (Direction data type)
parseInput :: SF (Event InputEvent) GameInput
parseInput = proc e -> do 
  keys <- dHold (G.EventResize (0,0)) -< e
  keys'<- arr k -< keys
  returnA -< keys'
 where
  k event = case event of 
    (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _) -> Types.Up
    (G.EventKey (G.SpecialKey G.KeyDown) G.Down _ _) -> Types.Down
    (G.EventKey (G.SpecialKey G.KeyLeft) G.Down _ _) -> Types.Left
    (G.EventKey (G.SpecialKey G.KeyRight) G.Down _ _) -> Types.Right
    _ -> None


-- | Our main signal function which is responsible for handling the whole
-- game process, starting from parsing the input, moving to the game logic
-- based on that input and finally drawing the resulting game state to
-- Gloss' Picture
mainSF :: StdGen -> SF (Event InputEvent) G.Picture
mainSF g = parseInput >>> wholeGame g >>> drawGame

playGame :: IO ()
playGame =do
  do
    g <- newStdGen
    playYampa
        (G.InWindow "Yampa Example" (420, 360) (800, 600))
        G.white
        60
        (mainSF g)

main = playGame
