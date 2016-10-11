module Types where

import FRP.Yampa
import Graphics.Gloss
import qualified Graphics.Gloss.Interface.IO.Game as G

import System.Random
data Player = Player {
   imageSrc    :: String
  ,position :: (Int,Int)
  ,dir      :: Direction
  ,score    :: Int
} deriving (Show)

data Board = Board {
   player1 :: Player
} deriving (Show)

--not likely to change much below here
data GameState = GameState { board :: Board
                           , status :: GameStatus
                           , gen :: StdGen
                           } deriving (Show)

data Direction = Up | Down | Left | Right | None deriving (Eq, Show)
data GameStatus = InProgress
                | GameOver
                deriving (Eq, Show)

type GameInput = Event Direction
type InputEvent = G.Event


