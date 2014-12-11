> {-# LANGUAGE Arrows #-}
> module Main where
> import Euterpea
> import Euterpea.Experimental

> import Control.Arrow
> import Control.Concurrent
> import Control.Concurrent.STM
> import Control.Monad
> import System.IO.Unsafe

> import ExperimentalMidi

> import Flappy

STM

> type VolChan = TVar Double --TChan Double
> type PanChan = TVar (Double,Double)
> type DevChan = TVar (Int,Int)



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

> midi :: IO ()
> midi = do
>  v <- newTVarIO 0.2
>  setNumCapabilities v
>  forkOn 1 $ flap v
>  forkOn 2 $ midiInLoop v
>  return ()





 forkOn 2 $ outFile "test.wav" 500 $ sfReader v
 forkOn 2 $ forever $ (atomically $ isEmptyTChan v) >>= print
 forkOn 2 $ forever $ (atomically $ readTVar d) >>= print
