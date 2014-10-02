> {-# LANGUAGE Arrows #-}

A way to specify a location on the 2D plane around a use and spatialize sound
TODO - write the file

> module Spatializer where
> import Euterpea
> import Control.Arrow

create a stereo a signal from a mono signal
  
> stereoSplit :: AudSF (Double) (Double, Double)
> stereoSplit =  arr (\mono -> (mono,mono))

and adjust volume seperatly ideally
hmm, this should use some kind of split maybe...
maybe not...

> stereoPan :: Double -> Double -> AudSF (Double,Double) (Double, Double)
> stereoPan  a b =
>    proc (l,r) -> do
>       outA -< (l*a,r*b)

chec
we want to delay the sound to the "farther away" ear by 
the speed of sound
take a stereo signal and delay one (both, with one being 0) line

> delayStereo :: Double -> Double -> AudSF (Double, Double) (Double, Double)
> delayStereo l_delay r_delay = 
>	proc (l,r) -> do
>     l' <- delayLine l_delay -< l
>     r' <- delayLine r_delay -< r
>     outA -< (l',r')

> l = 0.9
> r = 0.1
> runme3 = outFile "test.wav" 10 
>			((oscFixed 440 &&& oscFixed 330) >>> (stereoPan l r) >>> (delayStereo l r))
