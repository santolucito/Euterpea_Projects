{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types.GameObjs where

import Types.Common
import Control.Lens (makeLenses)
import System.Random

data GameObj = GameObj {
   _position :: (Int,Int)
  ,_display :: Bool
  ,_img :: FilePath
} deriving (Show)

data Player = Player {
  _position   :: (Int,Int)
  ,_dir        :: Direction
  ,_score      :: Int
  ,_aliveTime  :: Int
  ,_inMotion   :: Bool
} deriving (Show)

data Level = Level String
data Board = Board {
    _player1  :: Player
  ,_levelName :: Level
}


data Images = Images {
  _playerImgs :: ImageMap
 ,_levelImgs :: ImageMap 
 }


data GameState = GameState { 
     _board :: Board
   , _status :: GameStatus
   , _gen :: StdGen
   , _images :: Images
   }

makeLenses ''GameState
makeLenses ''Board
makeLenses ''Player
makeLenses ''Images

