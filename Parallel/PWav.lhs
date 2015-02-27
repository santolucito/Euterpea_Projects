> {-# LANGUAGE Arrows #-}
> module Main where
> import Euterpea
> import Euterpea.Experimental

> import Control.Arrow
> import Control.Concurrent
> import Control.Concurrent.STM
> import Control.Monad
> import System.IO.Unsafe

 import ExperimentalMidi

> type VolChan = TVar Double --TChan Double
> type PanChan = TVar (Double,Double)
> type DevChan = TVar (Int,Int)


--------------------------
This file attempts to get two files playing cleanly and
have a gui to control volume

--------------------------
apparently every uisf widget thing needs a unique title

> volume_slider :: String -> UISF () (Double)
> volume_slider t = proc _ -> do
>    a <- title t $ vSlider (0,1) 0 -< ()
>    _ <- display -< 1-a
>    outA -< 1-a

> pan_slider :: UISF () (Double,Double)
> pan_slider = proc _ -> do
>    a <- title "pan"  $ hSlider (0,1) 0 -< ()
>    _ <- display -< (a,1-a)
>    outA -< (a,1-a)

> uisfWriter :: TVar a -> UISF (a) ()
> uisfWriter v = liftAIO (\x -> atomically $ writeTVar v x)

> mixer_board :: VolChan -> VolChan -> UISF () ()
> mixer_board vc pc = title "Mixer" $ leftRight $ proc _ -> do
>    v <- volume_slider "track1" -< ()
>    _ <- uisfWriter vc -< v
>    v2 <- volume_slider "track2" -< ()
>    _ <- uisfWriter pc -< v2
>    returnA -< ()

mi::Int

Audio

> toMono :: AudSF (Double, Double) (Double)
> toMono =  arr (\(l,r) -> (l+r)/2)

> -- volume_control :: AudioSample a => AudSF (a, Double) (a)
> volume_control :: AudSF ((Double,Double), Double) (Double,Double)
> volume_control = arr (\((l,r),v) -> (l*v,r*v))

> pan :: AudSF ((Double,Double), (Double,Double)) (Double,Double)
> pan = arr (\((l,r),(v1,v2)) -> (l*v1,r*v2))

> mix2 :: AudSF ((Double,Double), (Double,Double)) (Double,Double)
> mix2 = arr (\((l,r),(v1,v2)) -> ((l+v1)/2,(r+v2)/2))

> -- sfReader :: (Arrow a, Chan b) => Chan b -> (a () b)
> sfReader :: TVar a -> (AudSF () a)
> sfReader v =  arr (\x -> unsafePerformIO $ atomically $ readTVar v)


> wavloop :: VolChan -> VolChan -> IO ()
> wavloop v v2 = do
>   foo <- wavSFStereoInf "in1.wav"
>   bar <- wavSFStereoInf "in2.wav"
>   let sigPlay = ((((foo) &&& sfReader v) >>> volume_control) &&&
>                  (((bar) &&& sfReader v2) >>> volume_control)) >>> mix2 >>> toMono
>   playSignal 1000 sigPlay


> main' :: IO ()
> main' = do
>  v <- newTVarIO 0.2
>  v2 <- newTVarIO 0.2
>  p <- newTVarIO (1,1)
>  setNumCapabilities 2
>  forkOn 1 $ runMUI (defaultMUIParams {uiSize=(300,300), uiTitle="Instrument Demo"}) (mixer_board v v2)
>  forkOn 2 $  wavloop v v2
>  return ()

> main :: IO ()
> main = do
>  v <- newTVarIO 0.2
>  v2 <- newTVarIO 0.2
>  p <- newTVarIO (1,1)
>  runMUI (defaultMUIParams {uiSize=(300,300), uiTitle="Instrument Demo"}) (mixer_board v v2)
>  forkIO $ wavloop v v2
>  return ()
