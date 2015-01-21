> {-# LANGUAGE Arrows #-}


> module Main where
> import Euterpea
> import Euterpea.ExperimentalPlay
> import Control.Monad

> import Control.Concurrent.STM
> import System.IO.Unsafe
> import Control.Concurrent
> import FRP.UISF.AuxFunctions

> import Breakout

> main :: IO ()
> main = do
>  v <- atomically $ newTVar 0.0
>  setNumCapabilities 2
>  forkOn 1 $ game v
>  forkOn 2 $ breakSound v
>  return ()

--------
sound

> readT :: TVar a -> a
> readT x = unsafePerformIO $ atomically $ readTVar x

> breakSound :: TVar Double -> IO()
> breakSound v = play' $ line $ foo v

this works because of lazy eval
we won't calculate the music value until we need to actually play it
hence we have realtime composition
For some reason, using a rest breaks make it stop playing after playing one rest

> foo :: TVar Double -> [Music (Pitch, Volume)]
> foo v | s => 0.99 = (addVolume 127 (e 5 (1/4)) ) : foo v
>       | otherwise  = (addVolume 0 (c 4 (1/100)) ) : foo v
>    where
>       s = readT v

---------
visual

> game' v = runMUI (defaultMUIParams {uiSize=(300,300), uiTitle="Instrument Demo"})
>                (mixer_board v)

> volume_slider :: String -> UISF () (Double)
> volume_slider t = proc _ -> do
>    a <- title t $ vSlider (0,1) 0 -< ()
>    _ <- display -< 1-a
>    outA -< 1-a

> uisfWriter :: TVar Double -> UISF (Double) ()
> uisfWriter v = liftAIO (\x -> atomically $ writeTVar v x)

> mixer_board :: TVar Double -> UISF () ()
> mixer_board vc = title "Mixer" $ leftRight $ proc _ -> do
>    v <- volume_slider "track1" -< ()
>    _ <- uisfWriter vc -< v
>    returnA -< ()
