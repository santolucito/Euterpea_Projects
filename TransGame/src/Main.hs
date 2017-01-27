{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}

module Main where

import GameController
import Types.Types

import Render.Render 
import Render.GlossInterface
import Render.ImageIO

import Input.Input

import FRP.Yampa (Event(..), SF, (>>>))
import qualified Graphics.Gloss.Interface.IO.Game as G

import System.Random (newStdGen, StdGen)


main = playGame

-- | load a random numbe gen
-- and load up all the images we might need
-- this might be ok b/c lazy, but will have to check later
playGame :: IO ()
playGame =do
  do
    g <- newStdGen

    levelImgs <- makeLevelImgMap
    playerImgs <- makePlayerImgMap
    
    playYampa
        (G.InWindow "Yampa Example" (420, 360) (800, 600))
        G.white
        60
        (mainSF g (Images {_playerImgs=playerImgs, _levelImgs=levelImgs}))

-- | Our main signal function which is responsible for handling the whole
-- game process, starting from parsing the input, moving to the game logic
-- based on that input and finally drawing the resulting game state to
-- Gloss' Picture
mainSF :: StdGen -> Images -> SF (Event InputEvent) G.Picture
mainSF g is = 
  parseInput >>> wholeGame g is >>> drawGame
