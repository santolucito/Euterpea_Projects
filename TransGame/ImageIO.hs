module ImageIO where

import Types
import Codec.Picture
import qualified Data.Map as M

import Graphics.Gloss.Juicy
import qualified Graphics.Gloss.Interface.IO.Game as G

import Control.Lens
import Debug.Trace

playerImgSrcs = map ("pics/"++) ["Up.gif","Right.gif","Down.gif","Left.gif"]
-- for procedurally generated images, make level list with some code
levelImgSrcs = map ("pics/"++) ["mazeCircle.png"]

makePlayerImgMap :: IO(ImageMap)
makePlayerImgMap = makeImgMap playerImgSrcs

makeLevelImgMap :: IO(ImageMap)
makeLevelImgMap = makeImgMap levelImgSrcs

makeImgMap :: [String] -> IO(ImageMap)
makeImgMap is = do
 allImages <- mapM readImage is
 let imgs = map (either blackImage convertRGBA8) allImages
 let pics = map fromImageRGBA8 imgs --TODO: or is 'loadJuicy is' better?
 let toMap ks vs = M.fromList $ zip ks vs
 return $ toMap is (zip imgs pics)


getPlayerPic :: ImageMap -> Player -> G.Picture
getPlayerPic playerImgs p= let
  d = view dir p
  i = playerImgs M.! ("pics/"++(show d)++".gif")
 in
  snd i

getLevelImg :: GameState -> Image PixelRGBA8
getLevelImg g = let
  is = view (images.levelImgs) g
  iName =  ("pics/"++(view (board.levelName) g)++".png")
  i = is M.! iName
 in
  fst i
