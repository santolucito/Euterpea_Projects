{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.GameObjs where

import Types.Common
import Control.Lens (makeLenses)
import System.Random
import Data.HashSet
import Data.Hashable
import GHC.Generics

data GameObj = GameObj {
   _position :: (Int,Int)
  ,_display :: Bool
  ,_img :: FilePath
} deriving (Show,Eq,Generic)

instance Hashable GameObj

data Player = Player {
  _gameObj     :: GameObj
  ,_dir        :: Direction
  ,_score      :: Int
  ,_aliveTime  :: Int
  ,_inMotion   :: Bool
} deriving (Show)


data Level = Level String
data Board = Board {
   _player1   :: Player
  ,_levelName :: Level
  ,_objs      :: HashSet GameObj
}



data GameState = GameState { 
     _board :: Board
   , _status :: GameStatus
   , _gen :: StdGen
   , _images :: ImageMap
   }

makeLenses ''GameObj
makeLenses ''GameState
makeLenses ''Board
makeLenses ''Player

