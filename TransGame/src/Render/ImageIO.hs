{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
module Render.ImageIO where

import Types.Common
import Types.GameObjs
import Types.HasImage
import Settings

import qualified Graphics.Gloss.Interface.IO.Game as G
import Graphics.Gloss.Juicy
import Codec.Picture

import qualified Data.Map as M

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
getImg :: HasImageSrc a => (Board -> a) -> (Images -> ImageMap) -> GameState -> (Image PixelRGBA8,G.Picture)
getImg obj all g = let
  o = obj $ _board g
  s = getImageSrc o
  allImgs = all $ _images g
 in
  allImgs M.! (Settings.imageDir ++ s)


  
