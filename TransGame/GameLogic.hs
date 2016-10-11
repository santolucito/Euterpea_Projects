module GameLogic where

import System.Random (StdGen)

import Types
import FRP.Yampa



initialState :: StdGen -> GameState 
initialState g = GameState { 
   board = emptyBoard
  ,status = InProgress
  ,gen = g
}

emptyBoard = Board {player1 = Player {
   imageSrc =  "pics/stand_east.png"
  ,position = (0,0)
  ,dir      = Types.Left
  ,score    = 0}
}

isGameOver :: GameState -> Bool
isGameOver s = False

update :: (GameState, GameInput) -> GameState
update (gameState, input) =
    case input of
      Event direction -> gameState
      _ -> id gameState


