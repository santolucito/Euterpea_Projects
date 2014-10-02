> {-# LANGUAGE Arrows #-}

A way to specify a location on the 2D plane around a use and spatialize sound
This is actually really complicated and needs integration of a database
http://sound.media.mit.edu/resources/KEMAR.html

for now we just play with delays based on speed of sound, oh well


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

we want to delay the sound to the "farther away" ear by 
the speed of sound
take a stereo signal and delay one (both, with one being 0) line

> delayStereo :: (Double, Double) -> AudSF (Double, Double) (Double, Double)
> delayStereo (l_delay, r_delay) = 
>	proc (l,r) -> do
>     l' <- delayLine l_delay -< l
>     r' <- delayLine r_delay -< r
>     outA -< (l',r')

Given a point distance away from center of person's head
(meters,meters), how much how long will it take for sound to 
arrive at each ear

> type Point = (Double,Double) 

> earTime :: Point -> (Double, Double)
> earTime p1 =
>   let 
>     p2 = (0.08, 0)
>     p3 = (-0.08, 0)
>     p1p2 = distance p1 p2
>     p1p3 = distance p1 p3
>   in
>     (p1p2/340,p1p3/340)

> distance :: Point -> Point -> Double
> distance p1 p2 =
>   let
>     x=fst p1-fst p2
>     y=snd p1-snd p2
>   in
>     sqrt(x*x + y*y)

> l = 0.7
> r = 0.3
> runme3 = outFile "test.wav" 10 
>          (oscFixed 440 >>> stereoSplit >>> (stereoPan l r)
>           >>> (delayStereo $ earTime (-3,1)))
