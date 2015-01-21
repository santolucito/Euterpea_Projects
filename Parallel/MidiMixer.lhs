> {-# LANGUAGE Arrows, NoImplicitPrelude, QuasiQuotes, DataKinds #-}


> module Main where
> import Euterpea
> import Euterpea.ExperimentalPlay
> import Control.Monad

> import Control.Concurrent.STM
> import System.IO.Unsafe
> import Control.Concurrent
> import FRP.UISF.AuxFunctions
> import Control.Applicative

> import BasePrelude
> import Record
> import Record.Lens

> type XData =
>  [r| {c :: [Int], e :: [Int]} |]

> defXData = [r| {c=[],e=[]} |]

> getC :: XData -> [Int]
> getC = view [lens|c|]
> setC :: [Int] -> XData -> XData
> setC = set [lens|c|]

> main :: IO ()
> main = do
>  v <- atomically $ newTVar defXData
>  setNumCapabilities 2
>  forkOn 1 $ game v
>  forkOn 2 $ breakSound v
>  return ()

--------
sound

> readT :: TVar a -> a
> readT x = unsafeDupablePerformIO $ atomically $ readTVar x

> breakSound :: TVar XData -> IO()
> breakSound v =
>    play' $ line $ foo v

this works because of lazy eval
we won't calculate the music value until we need to actually play it
hence we have realtime composition
For some reason, using a rest breaks make it stop playing after playing one rest

> foo :: TVar XData -> [Music (Pitch, Volume)]
> foo v =
>    let
>      s = readT v
>      n = replicate 4 (e 5 (1/8))
>      m = zipWith (\x y -> addVolume (127*x) y) (getC s) n
>    in
>      m ++  foo v

---------
visual

> game v = runMUI (defaultMUIParams {uiSize=(300,300), uiTitle="Instrument Demo"})
>                (mixer_board v)

> boxes :: String -> UISF () ([Int])
> boxes t = title t $ proc _ -> do
>   w <- checkbox "0" True -< ()
>   x <- checkbox "1" True -< ()
>   y <- checkbox "2" True -< ()
>   z <- checkbox "3" True -< ()
>   outA -< bin w ++ bin x ++ bin y ++ bin z
>   where
>     bin True = [1]
>     bin False = [0]

> uisfWriter :: TVar a -> UISF (a) ()
> uisfWriter v = liftAIO (\x -> atomically $ writeTVar v x)

> uisfReader :: TVar a -> UISF () (a)
> uisfReader v = liftAIO (\x -> atomically $ readTVar v)

> mixer_board :: TVar XData -> UISF () ()
> mixer_board vc = title "Mixer" $ leftRight $ proc _ -> do
>    o <- uisfReader vc -< ()
>    v <- boxes "E 5"-< ()
>    v2 <- boxes "C 3" -< ()
>    _ <- uisfWriter vc -< (setC v o)
>    returnA -< ()
