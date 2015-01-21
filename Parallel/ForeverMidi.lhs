> {-# LANGUAGE Arrows #-}


> module Main where
> import Euterpea
> import Euterpea.ExperimentalPlay
> import Control.Monad

> import Control.Concurrent.STM
> import System.IO.Unsafe
> import Control.Concurrent
> import FRP.UISF.AuxFunctions


 import Breakout




> main :: IO ()
> main = do
>  v <- newTVarIO True
>  setNumCapabilities 2
>  forkOn 1 $ game v
>  forkOn 2 $ breakSound v
>  return ()

--------
sounds

> readT :: TVar a -> a
> readT x = unsafePerformIO $ atomically $ readTVar x

> breakSound :: TVar Bool -> IO()
> breakSound v = play' $ line $ foo v

> foo :: TVar Bool -> [Music Pitch]
> foo v = if readT v then (c 4 (1/2)) : foo v
>                    else (d 4 (1/2)) : foo v

---------
visual

> game v = runMUI (defaultMUIParams {uiSize=(300,300), uiTitle="Instrument Demo"})
>                (mixer_board v)

> volume_slider :: String -> UISF () (Double)
> volume_slider t = proc _ -> do
>    a <- title t $ vSlider (0,1) 0 -< ()
>    _ <- display -< 1-a
>    outA -< 1-a

> uisfWriter :: TVar Bool -> UISF (Double) ()
> uisfWriter v = liftAIO (\x -> atomically $ writeTVar v (if x>0.5 then True else False))

> mixer_board :: TVar Bool -> UISF () ()
> mixer_board vc = title "Mixer" $ leftRight $ proc _ -> do
>    v <- volume_slider "track1" -< ()
>    _ <- uisfWriter vc -< v
>    returnA -< ()
