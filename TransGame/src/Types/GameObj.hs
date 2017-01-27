module Types.GameObj where

data GameObj = GameObj {
   _position :: (Int,Int)
  ,_display :: Bool
  ,_img :: FilePath
} deriving (Show)

{-
collision :: GameObj -> GameObj -> Bool
collision obj obj'  = 
  let p = _position obj 
 in
  

wallCollision :: GameState -> Bool
wallCollision g = let
  (x,y) = view (board.player1.position) g
  xsize = 5
  ysize = 8
  playerLocs = [(x',y') | x' <- [x-xsize..x+xsize],y' <- [y-ysize.. y+ysize]]
  boardPixels = map (\(x,y) -> pixelAtFromCenter (getLevelImg g) x y) playerLocs
 in 
  any (==blackAPixel) boardPixels

pixelAtFromCenter :: Image PixelRGBA8 -> Int -> Int -> PixelRGBA8
pixelAtFromCenter i x y = let
  h = imageHeight i 
  w = imageWidth i 
  x' = (w `div` 2) + x
  y' = (h `div` 2) + (-y)
 in
  pixelAt i x' y'
  --whitePixel
-}
