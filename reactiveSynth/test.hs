{-# LANGUAGE Arrows #-}
module Main where

import FRP.Yampa

import FFI
import Types

import           Control.Monad
import Control.Concurrent

import           Data.Time.Clock.POSIX

main :: IO ()
main = reactimate
    (return 'a')
    sense
    action
    sigFun

sense :: Bool -> IO (DTime, Maybe Char)
sense _ = do
  startT <- getPOSIXTime
  --threadDelay 100000
  --x <- pullOneCharInput
  x <- getHiddenChar
  endT <- liftM2 (-) getPOSIXTime (return startT)
  return (fromRational $ toRational endT, Just x)

action :: Bool -> Out -> IO Bool
action _ x = putStrLn x >> return False

sigFunDelay :: SF Char Out
sigFunDelay = proc i -> do
  t <- time -< i
  s <- delay 1 'a' -< i
  o <- arr (\(i,s) -> if i==s then '!' else i) -< (i,s)
  returnA -< ([o]++ " "++[s]++" "++show t)

sigFun :: SF Char Out
sigFun = proc i -> do
  totalTime <- time -< i
  lastTime <- iPre 0 -< totalTime
  rec
    d <- iPre 'a' -< o
    o <- arr (\(i,s,tDelta) -> if i==s && tDelta<0.5 then '!' else i) -< (i,d,totalTime-lastTime)
  returnA -< ([o]++ " "++[d]++" "++show (totalTime-lastTime))
