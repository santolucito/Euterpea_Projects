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

sigFun :: SF Char Out
sigFun = proc i -> do
  t <- time -< i
  s <- delay 1 'a' -< i
  o <- arrPrim (\(i,s) -> if i==s then '!' else i) -< (i,s)
  returnA -< ([o]++ " "++[s]++" "++show t)
