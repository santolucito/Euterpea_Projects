{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- | Send the depth information in a concensed form over a data channel

module KinectModule
  where

import           Control.Concurrent
import           Control.Monad
import           Data.Bits
import           Data.IORef
import           Data.Vector.Storable      (Vector,(!))
import qualified Data.Vector.Storable      as V
import           Data.Word
import           Foreign.ForeignPtr
import           Freenect
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT          hiding (shift)
import           System.Process            (system)

import Control.Concurrent.STM
import System.IO.Unsafe

width, height :: Int
width = 640
height = 480

kinect_start :: TVar Int -> IO ()
kinect_start dataChan = do
  depthGrid <- newIORef Nothing
  _ <- getDepthThread depthGrid
  forever $ display depthGrid dataChan

-- | this wil start up the process of getting events from the kinect
getDepthThread :: IORef (Maybe (Vector Word16)) -> IO ThreadId
getDepthThread depthGrid = forkOS $ do
  withContext $ \context -> do
    setLogLevel LogFatal context
    selectSubdevices context devices
    withDevice context index $ \device -> do
      setDepthMode device Medium ElevenBit
      setDepthCallback device $ \payload _timestamp -> do
        writeIORef depthGrid (Just payload)
        postRedisplay Nothing
        return ()
      startDepth device
      forever $ processEvents context

  where devices = [Camera]
        index = 0 :: Integer


-- | display is a misnomer, this is just taking the data from kinect (in the IORef)
--   doing some calculations to reduce it to a single int value
--   then passing that into the dataChannel (TVar) to be used by the sound module

display ::  IORef (Maybe (Vector Word16)) -> TVar Int -> IO ()
display depthGrid dataChan = do
  depthGrid <- readIORef depthGrid
  case depthGrid of
    Nothing -> return ()
    Just grid -> do
      let coords = [(x,y) | x <- [0..width-1], y <- [0..height-1]]
      count <- forFold 0 coords $ \count (x,y) -> do
        let rawDisparity = fromIntegral (grid ! (y*width + x))
            d = rawDisparity/n
        if d<0.8
          then if d<0.5
                  then do patch (x,height-y) (1,d,d); return (count+1)
                  else do patch (x,height-y) (d,d,d); return count
          else return count
      -- HERE we send final reduced value to dataChan
      atomically $ writeTVar dataChan count
  where forFold nil xs cons = foldM cons nil xs

-- the resolution
n=2^11 - 1000

type PatchColor = (GLfloat,GLfloat,GLfloat)
type Loc = (Int,Int)

patch :: Loc -> PatchColor -> IO ()
patch (x,y) (r,g,b) = do
  color $ Color3 r g b
  rect (Vertex2 xf yf) (Vertex2 (xf+1) (yf+1))
  where xf = fromIntegral x :: GLfloat
        yf = fromIntegral y :: GLfloat
