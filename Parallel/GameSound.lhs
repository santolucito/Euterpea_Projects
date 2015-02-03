> {-# LANGUAGE Arrows #-}


> module Main where
> import Euterpea
> import Euterpea.ExperimentalPlay
> import Control.Monad

> import Control.Concurrent.STM
> import System.IO.Unsafe
> import Control.Concurrent
> import FRP.UISF.AuxFunctions
> import Control.Applicative

> import Breakout

> main :: IO ()
> main = do
>  v <- atomically $ newTVar (0,0.0)
>  setNumCapabilities 2
>  forkOn 2 $ breakSound v
>  game v
>  return ()

--------
sound

> readT :: TVar a -> a
> readT x = unsafeDupablePerformIO $ atomically $ readTVar x

> breakSound :: TVar (Double,Double) -> IO()
> breakSound v =
>    play' $ line $ foo v

this works because of lazy eval
we won't calculate the music value until we need to actually play it
hence we have realtime composition
For some reason, using a rest breaks make it stop playing after playing one rest
** other option is deliver midi event **

> foo :: TVar (Double,Double) -> [Music (Pitch, Volume)]
> foo v =
>    let
>      s = readT v
>      m = if fst s > 0
>          then (addVolume 127 $ note (1/10) $ pitch $ round $ (+40) $ (*20) $ (+1) $ snd s)
>          else (addVolume 0 (c 4 (1/10000)) )
>      _ = unsafeDupablePerformIO $ atomically $ writeTVar v (0,snd s)
>    in
>      m : foo v

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
