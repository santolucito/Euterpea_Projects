{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types.Types where

--import FRP.Yampa
import qualified Graphics.Gloss.Interface.IO.Game as G

import System.Random

import Control.Lens
import qualified Data.Map as M
import Codec.Picture

import Types.GameObj

data Player = Player {
   _position   :: (Int,Int)
  ,_dir        :: Direction
  ,_score      :: Int
  ,_aliveTime  :: Int
  ,_inMotion   :: Bool
} deriving (Show)

data Board = Board {
   _player1  :: Player
  ,_levelName:: String
} 

data Images = Images {
  _playerImgs :: ImageMap
 ,_levelImgs :: ImageMap 
 }

-- | the Image (fst) is used for look at the data of the picture
-- | the G.Picutre is used for rendering
type ImageMap = M.Map String (Image PixelRGBA8,G.Picture)

data GameState = GameState { _board :: Board
                           , _status :: GameStatus
                           , _gen :: StdGen
                           , _images :: Images
                           }


blackPixel = PixelRGB8 0 0 0
blackAPixel = PixelRGBA8 0 0 0 255
whitePixel = PixelRGB8 255 255 255
blackImage = (\_-> generateImage (\_ _ -> blackAPixel) 10 10)

data Direction = Up | Down | Left | Right | None deriving (Eq, Show)
data GameStatus = InProgress
                | GameOver
                deriving (Eq, Show)

makeLenses ''GameState
makeLenses ''Board
makeLenses ''Player
makeLenses ''Images

type GameInput = Direction
--type GameInput = Event Direction
type InputEvent = G.Event

