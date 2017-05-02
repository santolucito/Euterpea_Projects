module Types.HasImage where

 import Types.GameObjs

-- | tough to tell where this module stops and ImageIO begins
 class HasImageSrc a where
   getImageSrc :: a -> String
 
 -- | Instances for each gameObj we need to render
 instance HasImageSrc GameObj where
   getImageSrc o = _img o
 
 instance HasImageSrc Level where
   getImageSrc (Level s) = s

 instance HasImageSrc Player where
   getImageSrc p =  let
     time = _aliveTime p
     t = if _inMotion p then time else 0 
     d = _dir p
    in
     getGifFrame t 9 $ show d

--Assume every gif (test.gif) has been expanded to
 --test_0.gif, test_1.gif, etc
 --this is of course a terrible idea, but it should work
 getGifFrame :: Double -> Int -> FilePath -> FilePath
 getGifFrame time numFrames fdir = let
   thisFrame = (floor (time * 10)) `mod` numFrames
  in
   fdir ++"/frame_" ++ (show thisFrame) ++ "_delay-0.06s.gif"
   
  
 
