> {-# LANGUAGE Arrows #-}

> module HW7 where
>
> import Euterpea
> import Control.Arrow ((<<<),(>>>),arr)
> import Control.SF.AuxFunctions (fftA)
> import Data.Complex (Complex ((:+)),polar)
> import Data.Maybe (listToMaybe,catMaybes)

> foldSF :: Clock c => (a -> b -> b) -> b -> [SigFun c () a] -> SigFun c () b
> foldSF f b sfs =
>   foldr g (constA b) sfs where
>     g sfa sfb =
>       proc () -> do
>         s1 <- sfa -< ()
>         s2 <- sfb -< ()
>         outA -< f s1 s2


1) 

> tab1 = tableSinesN 4096 [1,1,1,1]
> tab2 = tableSinesN 4096 [1.0,0.5,0.33]


> vibrato ::   Clock c =>
>              Double -> Double -> SigFun c Double Double
> vibrato vfrq dep = proc afrq -> do
>   vib  <- osc tab1 0 -< vfrq
>   aud  <- osc tab2  0 -< afrq + vib * dep
>   outA -< aud


> electro :: Instr (Mono AudRate)
> -- Dur -> AbsPitch -> Volume -> AudSF () Double
> electro dur ap vol [] =
>   let f = apToHz ap
>       v = fromIntegral vol/100
>       d = fromRational dur
>       sfs = map (\p -> constA (f * p) >>> vibrato (1*p) (10/p))
>             [4,3,2,1]
>   in proc () -> do
>     a1 <- foldSF (+) 0 sfs -< ()
>     outA -< a1 * v/4

> problem_1 = outFile "test.wav" 10 (electro 10 (absPitch (C,5)) 100 [])

2)

> tabC = tableSinesN 4096 [1.0,0,0.75,0,0.14,0,0.5,0,0.12,0,0.17]

> clarinet :: Instr (Mono AudRate)
> -- Dur -> AbsPitch -> Volume -> AudSF () Double
> clarinet dur ap vol [] =
>   let f = apToHz ap
>       v = fromIntegral vol/100
>       d = fromRational dur
>   in proc () -> do
>     aenv <- envExponSeg [0,1,0.001] [0.003,d -0.003] -< ()
>     a1 <- osc tabC 0 -< f
>     outA -< a1 * aenv

> problem_2 = outFile "test.wav" 10 (clarinet 10 (absPitch (C,5)) 100 [])

3)

a)

> type AudSF a b = SigFun AudRate a b

gernates a tone that varies from .5 to 2x the input frq
at the rate of vfrq 

> s1 :: Clock c => Double -> SigFun c Double Double
> s1 vfrq  = 
>    proc tfrq -> do
>      vib <- osc tab1 0 -< vfrq
>      aud <- osc tab1 0 -< (tfrq * 1.25) + (vib * (tfrq * 0.75))
>      outA -< aud

> s2 :: AudSF () Double -- Clock c => SigFun c () Double
> s2 = proc () -> do
>        main <- osc tab1 0 -< 440
>        vib <- s1 0.25 -< 440
>        outA -< vib * main

> runme1 = outFile "test.wav" 6 s2

b) 

gernates a tone that varies from .5 to 2x the input frq
at a dynamic rate 

> s3 :: Clock c => SigFun c Double Double
> s3 = 
>    proc tfrq -> do
>      my_env <- envLine 0.25 16 200 -< ()
>      vib <- osc tab1 0 -< my_env
>      aud <- osc tab1 0 -< (tfrq * 1.25) + (vib * (tfrq * 0.75))
>      outA -< aud


> s4 :: AudSF () Double -- Clock c => SigFun c () Double
> s4 = proc () -> do
>        main <- osc tab1 0 -< 440
>        vib <- s3 -< 440
>        outA -< vib * main

> runme2 = outFile "test.wav" 16 s4

You actaully start to hear an interval of two pitches... 
i think, my ear really isnt that good

c)

> s5 :: AudSF () Double -- Clock c => SigFun c () Double
> s5 = proc () -> do
>        vib1 <- s1 0.25 -< 240
>        vib2 <- s1 0.15 -< 240
>        vib3 <- s1 0.4 -< 240
>        outA -< vib1 * vib2 * vib3 

> runme3 = outFile "test.wav" 16 s5