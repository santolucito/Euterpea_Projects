--  LANGUAGE TemplateHaskell #-}

module GameLogic where

import System.Random (StdGen)
import Prelude hiding (Left,Right)
import Types 
-- import FRP.Yampa

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
      None -> id gameState
      dir -> moveP dir gameState

moveP :: Direction -> GameState -> GameState
moveP d g = over (board.player1.position) (appT updateF) g
 where
  updateF = case d of
   Down  -> (0,-1)
   Up    -> (0,1)
   Left  -> (-1,0)
   Right -> (1,0)
   _     -> (0,0)
  appT (dx,dy) (x,y) = (x+dx,y+dy)
