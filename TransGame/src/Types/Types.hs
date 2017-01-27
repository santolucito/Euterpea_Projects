{-# LANGUAGE DuplicateRecordFields #-}

module Types.Types where

--import FRP.Yampa
import qualified Graphics.Gloss.Interface.IO.Game as G

import System.Random

import qualified Data.Map as M
import Codec.Picture

import Types.GameObj

data Player = Player {
   position   :: (Int,Int)
  ,dir        :: Direction
  ,score      :: Int
  ,aliveTime  :: Int
  ,inMotion   :: Bool
} deriving (Show)

data Level = Level String
data Board = Board {
   player1  :: Player
  ,levelName :: Level
}

class HasImageSrc a where
  getImageSrc :: a -> String


data Images = Images {
  playerImgs :: ImageMap
 ,levelImgs :: ImageMap 
 }

-- | the Image (fst) is used for look at the data of the picture
-- | the G.Picutre is used for rendering
type ImageMap = M.Map String (Image PixelRGBA8,G.Picture)

data GameState = GameState { board :: Board
                           , status :: GameStatus
                           , gen :: StdGen
                           , images :: Images
                           }


blackPixel = PixelRGB8 0 0 0
blackAPixel = PixelRGBA8 0 0 0 255
whitePixel = PixelRGB8 255 255 255
blackImage = (\_-> generateImage (\_ _ -> blackAPixel) 10 10)

data Direction = Up | Down | Left | Right | None deriving (Eq, Show)
data GameStatus = InProgress
                | GameOver
                deriving (Eq, Show)


type GameInput = Direction
--type GameInput = Event Direction
type InputEvent = G.Event
