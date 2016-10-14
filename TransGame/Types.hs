{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types where

--import FRP.Yampa
import qualified Graphics.Gloss.Interface.IO.Game as G

import System.Random

import Control.Lens
import qualified Data.Map as M
import Codec.Picture


data Player = Player {
   _imageSrc :: String
  ,_position :: (Int,Int)
  ,_dir      :: Direction
  ,_score    :: Int
} deriving (Show)

data Board = Board {
   _player1  :: Player
  ,_walls    :: [[Tile]]
  ,_image    :: String
  ,_imageData:: Image PixelRGB8
} 

whitePixel = PixelRGB8 0 0 0
whiteImage = (\_-> generateImage (\_ _ -> whitePixel) 1 1)

data PosStatus = Open | Obstacle

data Tile = Tile { 
   _imageSrc :: String
  ,_position :: (Int,Int) --center
  ,_size     :: Int
} deriving (Show)

--not likely to change much below here
data GameState = GameState { _board :: Board
                           , _status :: GameStatus
                           , _gen :: StdGen
                           }

data Direction = Up | Down | Left | Right | None deriving (Eq, Show)
data GameStatus = InProgress
                | GameOver
                deriving (Eq, Show)

makeLenses ''GameState
makeLenses ''Board
makeLenses ''Player

type GameInput = Direction
--type GameInput = Event Direction
type InputEvent = G.Event

