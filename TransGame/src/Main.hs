{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}

module Main where

import GameController
import Types.Common

import Render.Render 
import Render.GlossInterface
import Render.ImageIO

import Input.Input

import FRP.Yampa (Event(..), SF, (>>>))
import qualified Graphics.Gloss.Interface.IO.Game as G
import Data.Map (union)
import System.Random (newStdGen, StdGen)

main :: IO()
main = playGame

-- | load a random numbe gen
--   NB : read in every image we will ever need
--   this might use up too much memor if the game uses many images since we have no way to evict an image (I think)
playGame :: IO ()
playGame =do
  do
    g <- newStdGen

    levelImgs <- makeImgMap levelImgSrcs
    playerImgs <- makeImgMap playerImgSrcs
    --coinImg <- makeImgMap coinImgSrc
    
    let imgs = levelImgs `union` playerImgs
    playYampa
        (G.InWindow "Yampa Example" (420, 360) (800, 600))
        G.white
        60
        (mainSF g imgs)

-- | Our main signal function which is responsible for handling the whole
-- game process, starting from parsing the input, moving to the game logic
-- based on that input and finally drawing the resulting game state to
-- Gloss' Picture
mainSF :: StdGen -> ImageMap -> SF (Event InputEvent) G.Picture
mainSF g is = 
  parseInput >>> wholeGame g is >>> drawGame
