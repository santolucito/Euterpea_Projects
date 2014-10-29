> {-# LANGUAGE Arrows #-}

jsut try and play two samples
one after another


> module SampleSeq where
> import Euterpea
> import Euterpea.IO.Audio.PortAudioChannel
> import Control.Arrow

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
