> {-# LANGUAGE Arrows #-}

jsut try and play two samples
one after another


> module Main where
> import Euterpea
> import Euterpea.IO.Audio.PortAudioChannel
> import Control.Arrow
> import Control.Arrow.ArrowIO
> import Control.Concurrent

> import System.IO.Unsafe

 openChannel 100 100 >>= readChannel

> toMono :: AudSF (Double, Double) (Double)
> toMono =  arr (\(l,r) -> (l+r)/2)

import and play a file

> runme2 = wavSFStereo "input.wav" >>= playSignal 10

import and export a file

> runme3 = wavSFStereo "input.wav" >>= outFile "test.wav" 10

import a stereo wav, convert to mono and play a file

take #1)getting it to work

> my_arr1 = unsafePerformIO (wavSFStereo "input.wav") >>> toMono

> my_arr2 =
>   proc () -> do
>       a <- unsafePerformIO $ wavSFStereo "input.wav" -< ()
>       b <- toMono -< a
>       outA -< b

> runme5 = playSignal 10 my_arr1

learning monads

> my_arr3 = (wavSFStereo "input.wav") >>= (\x -> return (x >>> toMono))

> my_arr4 = do
>    x <- wavSFStereo "input.wav"
>    return (x >>> toMono)

 my_arr4_2 = do
    return ((return $ wavSFStereo "input.wav") >>> toMono)

 my_arr5 =
   let
     x = do
        return $ wavSFStereo "input.wav"
   in
      proc () -> do
        a <- toMono -< x
        outA -< a

> runme6 = wavSFInf "input2.wav" >>= playSignal 20

really need to start writing all these with numChans
instead of Double or (double,Double)

first is signal, second is volume

> volume_control :: AudSF (Double,Double) (Double)
> volume_control = arr (\(s,v) -> (s*v))

> volume_control' :: AudSF (Double,Double) (Double)
> volume_control' = proc (s,v) -> do
>    a <- arr id -< (s*v)
>    outA -< a

> volume_slider :: UISF () (Double)
> volume_slider = proc _ -> do
>    a <- title "volume"  $ vSlider (0,1) 0 -< ()
>    _ <- display -< 1-a
>    outA -< 1-a

 toAudSF :: UISF a b -> AudSF () b
toAudSF uisf = proc () -> do
    a <- arr id -< uisf
    outA -< a

> mixer_board :: UISF () ()
> mixer_board = title "Mixer" $ proc _ -> do
>    v <- volume_slider -< ()
> --   _ <- arrIO0 runme7 -< ()
>    returnA -< ()

> main :: IO ()
> main = do
>  setNumCapabilities 2
>  forkOn 1 $ runUI "UI Demo" mixer_board
>  forkOn 2 $ runme7
>  return ()

need to figure out how to use the UISF value now
will probably embed the audSF in UISF
also need to embed IO() (from playSignal) into either arrow or runUI
could also have runUI return a signal then play on another thread

> runme7 = wavSFInf "input2.wav" >>= playSignal 20
