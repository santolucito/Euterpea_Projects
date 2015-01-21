> {-# LANGUAGE Arrows #-}

  {-# LANGUAGE Arrows, NoImplicitPrelude, QuasiQuotes, DataKinds #-}


> module Main where
> import Euterpea
> import Euterpea.ExperimentalPlay
> import Control.Monad

> import Control.Concurrent.STM
> import System.IO.Unsafe
> import Control.Concurrent
> import FRP.UISF.AuxFunctions
> import Control.Applicative

import BasePrelude
import Record
import Record.Lens

type XData =
 [r| {c :: [Int], e :: [Int]} |]

defXData = [r| {c=[],e=[]} |]

getC :: XData -> [Int]
getC = view [lens|c|]
setC :: [Int] -> XData -> XData
setC = set [lens|c|]


> main :: IO ()
> main = do
>  v <- atomically $ newTVar ([[],[],[]])
>  setNumCapabilities 2
>  forkOn 1 $ game v
>  forkOn 2 $ breakSound v
>  return ()

--------
sound

> readT :: TVar a -> a
> readT x = unsafeDupablePerformIO $ atomically $ readTVar x

> breakSound :: TVar ([[Int]]) -> IO()
> breakSound v =
>    play' $ Modify (Instrument (toEnum 121)) $ line $ foo v

this works because of lazy eval
we won't calculate the music value until we need to actually play it
hence we have realtime composition
For some reason, using a rest breaks make it stop playing after playing one rest

> foo :: TVar ([[Int]]) -> [Music (Pitch, Volume)]
> foo v =
>    let
>      xData = readT v
>      listE = replicate 8 (g 5 (1/8))
>      es = line $ zipWith (\x y -> addVolume (127*x) y) (xData!!0) listE
>      listC = replicate 8 (e 5 (1/8))
>      cs = line $ zipWith (\x y -> addVolume (127*x) y) (xData!!1) listC
>      listC' = replicate 8 (e 4 (1/8))
>      cs' = line $ zipWith (\x y -> addVolume (127*x) y) (xData!!2) listC'
>    in
>      chord [es,cs,cs'] :  foo v

---------
visual

> game v = runMUI (defaultMUIParams {uiSize=(300,300), uiTitle="Instrument Demo"})
>                (mixer_board v)

> boxes :: String -> UISF () ([Int])
> boxes t = title t $ leftRight $ proc _ -> do
>   x <- concatA $ replicate 8 (checkbox "" False) -< cycle [()]
>   outA -< map bin x
>   where
>     bin True = 1
>     bin False = 0

> uisfWriter :: TVar a -> UISF (a) ()
> uisfWriter v = liftAIO (\x -> atomically $ writeTVar v x)

> mixer_board :: TVar ([[Int]]) -> UISF () ()
> mixer_board vc = title "Mixer" $ topDown $ proc _ -> do
>   x <- concatA $ replicate 3 (boxes "") -< cycle [()]
>   _ <- uisfWriter vc -< x
>   returnA -< ()
