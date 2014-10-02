> {-# LANGUAGE Arrows #-}

> module HW6 where
>
> import Euterpea
> import Control.Arrow ((<<<),(>>>),arr)
> import Control.SF.AuxFunctions (fftA)
> import Data.Complex (Complex ((:+)),polar,realPart)
> import Data.Maybe (listToMaybe,catMaybes)



> sinTab1,sinTab2 :: Table
> sinTab1 = tableSinesN 4096 [1]
> sinTab2 = tableSinesN 256 [1]
> sqrTab :: Table
> sqrTab = tableLinear 4096 1 [(0.5,1),(0,-1),(0.5,-1)]

> pTab :: Table
> pTab = tableLinear 4096 1 [(0,20),(10,20000)]


> s1 :: AudSF () Double -- Clock c => SigFun c () Double
> s1 = proc () -> do
>        mySig <- osc pTab 0 -< 0.1
>        osc sinTab1 0 -< mySig

> s2 :: AudSF () Double -- Clock c => SigFun c () Double
> s2 = proc () -> do
>        mySig <- osc pTab 0 -< 0.1
>        osc sinTab2 0 -< mySig

> runme1 = outFile "test.wav" 10 s1
> runme2 = outFile "test.wav" 10 s2

the table size of 256 is a less clean sound

19.5

> sinTab3 :: Table
> sinTab3 = tableSinesN 4096 [1.0,0.9,0.6]

> simpleInstr :: InstrumentName
> simpleInstr = Custom "Simple Instrument"

> myInstr1 :: Instr (AudSF () Double)
> myInstr1 dur ap vol [vfrq,dep] =
>   proc () -> do
>       vib  <- osc sinTab3  0 -< vfrq
>       aud  <- osc sinTab1  0 -< apToHz ap + vib * dep
>       outA -< aud


use the vibrato to create some really 
funky noise in the signal

> myInstr2 :: Instr (AudSF () Double)
> myInstr2 dur ap vol [vfrq,dep] =
>   proc () -> do
>       vib  <- osc sinTab3 0 -< vfrq
>       aud  <- osc sinTab1 0 -< apToHz ap + vib * dep
>       aud2 <- osc sqrTab 0 -< apToHz ap 
>       aud3 <- osc sinTab1 0 -< vfrq-24
>       outA -< (aud + aud2 + aud3) / 3

> myInstrMap = [(simpleInstr, myInstr1)]
> (dr, sf)  = renderSF mel myInstrMap
> main      = outFile "simple.wav" dr sf

> myInstrMap2 = [(simpleInstr, myInstr2)]
> (dr2, sf2)  = renderSF mel myInstrMap2
> main2      = outFile "simple.wav" dr2 sf2

> mel :: Music1
> mel =  
>   let  m = Euterpea.line [  na1 (c 4 en),   na1 (ef 4 en),  na1 (f 4 en), 
>                      na2 (af 4 qn),  na1 (f 4 en),   na1 (af 4 en), 
>                      na2 (bf 4 qn),  na1 (af 4 en),  na1 (bf 4 en),
>                      na1 (c 5 en),   na1 (ef 5 en),  na1 (f 5 en),
>                      na3 (af 5 wn) ]
>        na1 (Prim (Note d p))  = Prim (Note d (p,[Params [0, 0]]))
>        na2 (Prim (Note d p))  = Prim (Note d (p,[Params [5,10]]))
>        na3 (Prim (Note d p))  = Prim (Note d (p,[Params [5,20]]))
>   in instrument simpleInstr m
  
3. create a stereo signal


switching two signals back and forth
also oscfixed is equiv to next line below

> stereoWav :: AudSF () (Double, Double)
> stereoWav = 
>     proc () -> do
>       a <- oscFixed 440 -< ()
>       b <- osc sinTab1 0 -< 300
>       pan <- osc sinTab1 0 -< 0.1
>       outA -< (b*(abs pan-1)+a*pan,a*(abs pan-1)+b*pan)

> runme3 = outFile "test.wav" 10 stereoWav

20.3
Define a function mkSqWave::Int → Int → [Complex Double]
such that 'mkSqWave num n' is the sum of the first 
n terms of the Fourier series of a square wave, 
having num samples in the result.

> mkTerm :: Int -> Double -> [Complex Double]
> mkTerm num n = let f = 2 * pi /fromIntegral num
>   in [sin (n * f * fromIntegral i )/n :+ 0
>       | i <- [0, 1..num-1]]

> mkSqWave :: Int -> Int -> [Complex Double]
> mkSqWave num 0 = mkTerm num 1
> mkSqWave num n = zipWith (+) (mkSqWave num (n-1)) (mkTerm num (fromIntegral n*2+1))


Related to Exercise 20.3, define a function 
mkSqWaveSF :: Double -> Int -> AudSF () Double 
that “simulates” a square wave by summing together 
the odd harmonics as described in Equation 20.4.
That is, mkSqWaveSF f n is a signal source with 
fundamental frequency f that is the sum of the first 
n odd harmonics, each scaled appropriately.

ended up just using the real part,
gets close to a square wave actually, but
almost certainly not the correct way to do this...

> mkSqWaveSF :: Double -> Int -> AudSF () Double 
> mkSqWaveSF f n = let x = zip [0,0.01..1] (map realPart (mkSqWave 100 n)) in
>   proc () -> do
>     a <- osc (tableLinear 4096 1 x) 0 -< f
>     outA -< a

> s4 :: AudSF () Double
> s4 = proc () -> do
>        a <- mkSqWaveSF 440 100 -< ()
>        outA -< a

> runme4 = outFile "test.wav" 10 s4
