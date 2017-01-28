{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
module Render.ImageIO where

import Types.Types
import Settings

import qualified Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Juicy
import Codec.Picture

import qualified Data.Map as M
import Control.Lens (view)

import Debug.Trace

-- | Where in the file system do images come from
levelImgSrcs :: [FilePath]
levelImgSrcs = map ("pics/"++) [Settings.levelImageSrc]

playerImgSrcs :: [FilePath]
playerImgSrcs = let
  f d = map (\x-> d++"/frame_"++(show x)++"_delay-0.06s.gif") [0..9]
 in
  map ("pics/"++) (concatMap f  ["Right", "Down", "Left", "Up"])

-- | we need different images for differnet character states
--   use a map from state names (string for now) to image
makeImgMap :: [FilePath] -> IO(ImageMap)
makeImgMap is = do
 allImages <- mapM readImage is
 let imgs = map (either blackImage convertRGBA8) allImages
 let pics = map fromImageRGBA8 imgs --TODO: or is 'loadJuicy is' better?
 let toMap ks vs = M.fromList $ zip ks vs
 return $ toMap is (zip imgs pics)

-- | get the chacter state image given a player state
--   we also simulate a gif here
instance HasImageSrc Level where
  getImageSrc (Level s) = s
instance HasImageSrc Player where
  getImageSrc p =  let
    time = _aliveTime p
    t = if _inMotion p then time else 0 
    d = _dir p
   in
    getGifFrame t 9 $ show d

getImg :: HasImageSrc a => (Board -> a) -> (Images -> ImageMap) -> GameState -> (Image PixelRGBA8,G.Picture)
getImg obj all g = let
  o = obj $ _board g
  s = getImageSrc o
  allImgs = all $ _images g
 in
  allImgs M.! (Settings.imageDir ++ s)


--Assume every gif (test.gif) has been expanded to
--test_0.gif, test_1.gif, etc
--this is of course a terrible idea, but it should work
getGifFrame :: Int -> Int -> FilePath -> FilePath
getGifFrame time numFrames dir = let
  thisFrame = (floor (fromIntegral time / 4)) `mod` numFrames
 in
  dir ++"/frame_" ++ (show thisFrame) ++ "_delay-0.06s.gif"
  
  
