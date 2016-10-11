module GameLogic where

import System.Random (StdGen)

import Types
import FRP.Yampa

import Control.Lens

initialState :: StdGen -> GameState 
initialState g = GameState { 
   board = emptyBoard
  ,status = InProgress
  ,gen = g
}

emptyBoard = Board {player1 = Player {
   --imageSrc =  "pics/stand_east.png"
   imageSrc =  "pics/east.gif"
  ,position = (0,0)
  ,dir      = Types.Left
  ,score    = 0}
}

isGameOver :: GameState -> Bool
isGameOver s = False

update :: (GameState, GameInput) -> GameState
update (gameState, input) =
    case input of
      Event direction -> (_board._player1._position) .~
        gameState { 
          board = (board gameState) { 
            player1 = (player1 (board gameState)) {
              position = (0,snd position + 1) }}}
      _ -> id gameState


