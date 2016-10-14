module Types where

import FRP.Yampa
import Graphics.Gloss

data Block = Block {
   myColor :: Color
  ,currPos :: (Int,Int)
  ,origPos :: (Int,Int)
  }

defaultBlock :: Block 
defaultBlock = Block
  {myColor = black
  ,currPos = (0,0)
  ,origPos = (0,0)}

iBlock :: SF Block Block
iBlock = iPre defaultBlock

inOrig :: [Block] -> Bool
inOrig = all (\b-> currPos b == origPos b)  

data QState = Classic | Quantum | ReQuant deriving (Eq,Show)
