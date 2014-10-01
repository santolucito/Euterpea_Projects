> {-# LANGUAGE Arrows #-}

> module HW5 where
>
> import Euterpea
> import Control.Arrow ((<<<),(>>>),arr)

Use osc to generate some waves


> sqrTab, sawTab, triTab :: Table
> sqrTab = tableLinear 4096 1 [(0.5,1),(0,-1),(0.5,-1)]
> sawTab = tableLinear 4096 1 [(1,-1)]
> triTab = tableLinear 4096 1 [(0.5,-1),(0.5,1)]

> sqrWav :: AudSF () Double
> sqrWav = constA 100 >>> osc sqrTab 0
> sawWav :: AudSF () Double
> sawWav = constA 100 >>> osc sawTab 0
> triWav :: AudSF () Double
> triWav = constA 100 >>> osc triTab 0

> runme = outFile "test.wav" 5 triWav

Use envASR to shape them

> s2 :: AudSF () Double -- Clock c => SigFun c () Double
> s2 = let myEnvASR rise dur dec = envLineSeg [0,1,1,0,0] [rise,dur-rise-dec,dec,1] in
>        proc () -> do
>        a <- osc sqrTab 0 -< 440
>        env <- myEnvASR 1 3 1 -< ()
>        outA -< a * env
> 
> runme2 = outFile "test.wav" 5 s2
>

19.1

> sinTab,sinTab2,sinTab3 :: Table
> sinTab = tableSinesN 4096 [1]
> sinTab2 = tableSinesN 1024 [1]
> sinTab3 = tableSinesN 10 [1]

> s3 :: AudSF () Double -- Clock c => SigFun c () Double
> s3 = proc () -> do
>        osc sinTab 0 -< 440
> s4 :: AudSF () Double -- Clock c => SigFun c () Double
> s4 = proc () -> do
>        osc sinTab2 0 -< 440
> s5 :: AudSF () Double -- Clock c => SigFun c () Double
> s5 = proc () -> do
>        osc sinTab3 0 -< 440
> s6 :: AudSF () Double -- Clock c => SigFun c () Double
> s6 = proc () -> do
>        osc sinTab 0 -< 9000        

> runme3 = outFile "test.wav" 5 s3
> runme4 = outFile "test.wav" 5 s4
> runme5 = outFile "test.wav" 5 s5
> runme6 = outFile "test.wav" 5 s6

19.2

> trmTab :: Table
> trmTab = tableSinesN 4096 [1]

> tremoloClip :: Clock c => Double -> Double -> SigFun c Double Double
> tremoloClip tfrq dep = 
>      proc origSig -> do
>      trm2 <- osc trmTab 0 -< SigNum tfrq
>      outA -< origSig + (origSig * dep * trm2)

Here is a better version that doesnt cause clipping
and given the lack of clipping,
treats depth in a slightly more sensical way

> tremolo :: Clock c => Double -> Double -> SigFun c Double Double
> tremolo tfrq dep = 
>      proc origSig -> do
>      trm2 <- osc trmTab 0 -< tfrq
>      outA -< origSig * (1-(0.5 * dep)) + (origSig * (0.5 * dep) * trm2)

it is really too bad this next one doesn't work,
from a design prespective I like it a lot.
i should look more into how env works i think

> tremoloBad :: Clock c => Double -> Double -> SigFun c Double Double
> tremoloBad tfrq dep = 
>   let myEnvASR  = envLineSeg (cycle [1-dep,1+dep]) [tfrq..] in
>      proc origSig -> do
>      trm <- myEnvASR -< ()
>      outA -< origSig * trm
>
> s7 :: AudSF () Double
> s7 = s3 >>> tremolo 1 0.1
> runme7 = outFile "test.wav" 5 s7

19.4

soft limit of 90%, exceeding that means we
take add only half of that last 10%
will eliminate clipping upto 1.1
without imposing overall scaling

> fixClip :: Clock c => SigFun c Double Double
> fixClip = arr f where
>   f x = if abs x <= 0.9 then x
>         else (0.9 * signum x) + (signum x * (abs x -0.9) * 0.5)

> s8, s88 :: AudSF () Double
> s88 = s3 >>> tremoloClip 1 0.2
> s8 = s88 >>> fixClip
> runme8 = outFile "test.wav" 5 s8