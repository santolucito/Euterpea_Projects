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
import Data.Maybe
--import Debug.Trace

-- | Where in the file system do images come from
levelImgSrcs :: [FilePath]
levelImgSrcs = map ("pics/"++) [Settings.levelImageSrc,"coin.png"]


playerImgSrcs :: [FilePath]
playerImgSrcs = let
  f d = map (\x-> d++"/frame_"++(show x)++"_delay-0.06s.gif") [0..9]
 in
  map ("pics/"++) (concatMap f  ["Right", "Down", "Left", "Up"])

-- | we need different images for differnet character states
--   use a map from state names (string from file name) to image
makeImgMap :: [FilePath] -> IO(ImageMap)
makeImgMap fileNames = do
 allImages <- mapM readImage fileNames
 let imgs = map (either blackImage convertRGBA8) allImages
 let pics = map fromImageRGBA8 imgs --TODO: or is 'loadJuicy is' better?
 let toMap ks vs = M.fromList $ zip ks vs
 return $ toMap fileNames (zip imgs pics)

 -- | get the chacter state image given a player state
 --   we also simulate a gif here
getImg :: HasImageSrc a => GameState -> a -> (Image PixelRGBA8,G.Picture)
getImg g o = let
  s = getImageSrc o
  allImgs =  _images g
  myImg = M.lookup (Settings.imageDir ++ s) allImgs
 in
  fromMaybe (error (("Could not find image : "++ (show (Settings.imageDir++s)))++" \nWithin avaible image paths : "++show (M.keys allImgs))) myImg

  
