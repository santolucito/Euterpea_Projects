{-# TemplateHaskell #-}

module GameLogic where

import System.Random (StdGen)

import Types
import FRP.Yampa

import Control.Lens

initialState :: StdGen -> GameState 
initialState g = GameState { 
   _board = emptyBoard
  ,_status = InProgress
  ,_gen = g
}

emptyBoard = Board {_player1 = Player {
   --imageSrc =  "pics/stand_east.png"
   _imageSrc =  "pics/east.gif"
  ,_position = (0,0)
  ,_dir      = Types.Left
  ,_score    = 0}
}

isGameOver :: GameState -> Bool
isGameOver s = False

update :: (GameState, GameInput) -> GameState
update (gameState, input) =
    case input of
      Event direction -> over (board.player1.position._1) (+1) gameState
      _ -> id gameState


