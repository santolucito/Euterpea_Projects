{-# LANGUAGE TemplateHaskell #-}

module Types where

import FRP.Yampa
import qualified Graphics.Gloss.Interface.IO.Game as G

import System.Random

import Control.Lens

data Player = Player {
   _imageSrc    :: String
  ,_position :: (Int,Int)
  ,_dir      :: Direction
  ,_score    :: Int
} deriving (Show)

data Board = Board {
   _player1 :: Player
} deriving (Show)

--not likely to change much below here
data GameState = GameState { _board :: Board
                           , _status :: GameStatus
                           , _gen :: StdGen
                           } deriving (Show)

data Direction = Up | Down | Left | Right | None deriving (Eq, Show)
data GameStatus = InProgress
                | GameOver
                deriving (Eq, Show)

makeLenses ''GameState
makeLenses ''Board
makeLenses ''Player

type GameInput = Event Direction
type InputEvent = G.Event


