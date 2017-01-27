module Render.ImageIO where

import Types.Types
import Settings


import Codec.Picture

import Graphics.Gloss.Juicy
import qualified Graphics.Gloss.Interface.IO.Game as G

import qualified Data.Map as M
import Control.Lens


-- | Where in the file system do images come from
levelImgSrcs :: [FilePath]
levelImgSrcs = map ("pics/"++) [Settings.imageSrc]
playerImgSrcs :: [FilePath]
playerImgSrcs = let
  f d = map (\x-> d++"/frame_"++(show x)++"_delay-0.06s.gif") [0..9]
 in
  map ("pics/"++) (concatMap f  ["Right", "Down", "Left", "Up"])

-- | we need different images for differnet character states
--   use a map from state names (string for now) to image
makePlayerImgMap :: IO(ImageMap)
makePlayerImgMap = makeImgMap playerImgSrcs

makeLevelImgMap :: IO(ImageMap)
makeLevelImgMap = makeImgMap levelImgSrcs


makeImgMap :: [FilePath] -> IO(ImageMap)
makeImgMap is = do
 allImages <- mapM readImage is
 let imgs = map (either blackImage convertRGBA8) allImages
 let pics = map fromImageRGBA8 imgs --TODO: or is 'loadJuicy is' better?
 let toMap ks vs = M.fromList $ zip ks vs
 return $ toMap is (zip imgs pics)

-- | get the chacter state image given a player state
--   we also simulate a gif here
getPlayerImg :: ImageMap -> Player -> G.Picture
getPlayerImg playerImgs p= let
  time = view aliveTime p
  t = if view inMotion p then time else 0 
  d = view dir p
  i = playerImgs M.! ("pics/"++(getGifFrame t 9 $ show d))
 in
  snd i

getLevelImg :: GameState -> (Image PixelRGBA8,G.Picture)
getLevelImg g = let
  allImgs = view (images.levelImgs) g
  iName =  ("pics/"++(view (board.levelName) g)++".png")
  i = allImgs M.! iName
 in
  i

--Assume every gif (test.gif) has been expanded to
--test_0.gif, test_1.gif, etc
--this is of course a terrible idea, but it should work
getGifFrame :: Int -> Int -> FilePath -> FilePath
getGifFrame time numFrames dir = let
  thisFrame = (floor (fromIntegral time / 4)) `mod` numFrames
 in
  dir ++"/frame_" ++ (show thisFrame) ++ "_delay-0.06s.gif"
  
  
