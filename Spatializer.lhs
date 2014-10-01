> {-# LANGUAGE Arrows #-}

A way to specify a location on the 2D plane around a use and spatialize sound
TODO - write the file

> module Spatializer where
> import Euterpea
> import Control.Arrow

> sinTab1 :: Table
> sinTab1 = tableSinesN 4096 [1]

we want to delay the sound to the "farther away" ear by 
the speed of sound
take a stereo signal and delay one line

> delayStereo :: AudSF (Double, Double) (Double, Double)
> delayStereo = undefined

we want to create a stereo a signal (from a normal signal)
and adjust volume (seperatly ideally)

> stereoWav :: AudSF () (Double, Double)
> stereoWav = 
>     proc () -> do
>       a <- osc sinTab1 0 -< 440
>       b <- osc sinTab1 0 -< 300
>       pan <- osc sinTab1 0 -< 0.1
>       outA -< (b*(abs pan-1)+a*pan,a*(abs pan-1)+b*pan)

> runme3 = outFile "test.wav" 10 stereoWav
