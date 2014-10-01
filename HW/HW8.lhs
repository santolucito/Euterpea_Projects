> {-# LANGUAGE Arrows #-}
>
> module HW8 where
>
> import Euterpea
> import Control.Arrow ((<<<),(>>>),arr)
> import Control.SF.AuxFunctions (fftA)
> import Data.Complex (Complex ((:+)),polar)
> import Data.Maybe (listToMaybe,catMaybes)


> type AudSF a b = SigFun AudRate a b

Exercise 1
Violin plot

> tabV = tableSines3N 4096 [(1,5,0), (2,5.1,0), (3,4.3,0), (4,3.3,0),
>                           (5,4.2,0), (6,4.2,0), (7,3.2,0), (8,4.5,0),
>                           (9,3.0,0), (10,3.1,0), (11,3.2,0)]
> violin :: Instr (Mono AudRate)
> -- Dur -> AbsPitch -> Volume -> AudSF () Double
> violin dur ap vol [] =
>   let f = apToHz ap
>       v = fromIntegral vol/100
>       d = fromRational dur
>   in proc () -> do
>     aenv <- envExponSeg [0,1,0.001] [0.003,d -0.003] -< ()
>     a1 <- osc tabV 0 -< f
>     outA -< a1 * aenv

> problem_1 = outFile "test.wav" 10 (violin 10 (absPitch (A,5)) 100 [])


Exercise 2

1) a cylindrical tube open at both ends will reflect an inverse wave at the ends back into the cylinder as the wave reaches those open ends. The majority of the sound percieved will be from the open end, in addition to the resonance of the tube itself. 

2) a cylindrical tube closed at both ends will reflect the wave at the ends back into cylinder as the wave reaches those closed ends. The percieved sound will be heard through the resonance of the tube itself.

3) a narrow metal plate will function the same way as the closed tube.

Exercise 3

> tab1 :: Table
> tab1 = tableSinesN 4096 [1]


this little function will take a number 0-1 and 
map it to a freq of a chromatic note on the piano

> mapToNotes :: Double -> Double
> mapToNotes x = (2 ** (((fromIntegral(floor (x * 88)))-49)/12) * 440.0) 

> myScifi1 :: Instr (Mono AudRate)
> myScifi1 dur ap vol [] =
>   let v = fromIntegral vol /100
>   in proc () -> do
>       a1 <- noiseBLH 42 -< 8
>       a2 <- osc tab1 0 -< mapToNotes a1
>       outA -< a2 * v
>
> problem_3 = outFile "scifi1.wav" 10 (myScifi1 10 (absPitch (C , 5)) 100 [])


Exercise 4
an 8 channel equalizer
20-80 Hz
80-160 Hz
160-320 Hz
320-640 Hz
640-1280 Hz
1280-2560 Hz
2560-5120 Hz
5120-20000 Hz

> type EightTuple = (Double,Double,Double,Double,Double,Double,Double,Double)

this is a bit embarresing... 

> get1th (a,_,_,_, _,_,_,_) = a
> get2th (_,a,_,_, _,_,_,_) = a
> get3th (_,_,a,_, _,_,_,_) = a
> get4th (_,_,_,a, _,_,_,_) = a
> get5th (_,_,_,_, a,_,_,_) = a
> get6th (_,_,_,_, _,a,_,_) = a
> get7th (_,_,_,_, _,_,a,_) = a
> get8th (_,_,_,_, _,_,_,a) = a

> equalizer :: AudSF (Double, EightTuple) Double
> equalizer = 
>   proc (s, gains) -> do 
>     s <- filterBandStopBW -< (s,50,(get1th gains)*  30)
>     s <- filterBandStopBW -< (s,120,(get2th gains)* 40)
>     s <- filterBandStopBW -< (s,240,(get3th gains)* 80)
>     s <- filterBandStopBW -< (s,480,(get4th gains)* 160)
>     s <- filterBandStopBW -< (s,960,(get5th gains)* 320)
>     s <- filterBandStopBW -< (s,1920,(get6th gains)*640)
>     s <- filterBandStopBW -< (s,3840,(get7th gains)*1280)
>     s <- filterBandStopBW -< (s,7680,(get8th gains)*2560)
>     outA -< s

> sweep :: AudSF () Double
> sweep = osc tab1 0 <<< envExpon 20 10 20000

> staticGains :: AudSF () EightTuple
> staticGains = constA (1,1,1,1, 1,1,1,1)

> stereoWav :: AudSF () Double
> stereoWav = 
>     proc () -> do
>       a <- sweep -< ()
>       b <- staticGains -< ()
>       c <- equalizer -< (a,b)
>       outA -< c

> problem_4 = outFile "test.wav" 10 stereoWav

Exercise 5 & 6

making a better clarinet

> tabC = tableSinesN 4096 [1.0,0,0.75,0,0.14,0,0.5,0,0.12,0,0.17]

> clarinet :: Instr (Mono AudRate)
> -- Dur -> AbsPitch -> Volume -> AudSF () Double
> clarinet dur ap vol [] =
>   let f = apToHz ap
>       v = fromIntegral vol/100
>       d = fromRational dur
>   in proc () -> do
>     aenv <- envASR 0.1 d 0.9 -< ()
>     a1 <- osc tabC 0 -< f

low pass filter here to simluate horn of bell

>     cutoff <- constA 300 -< ()
>     a1 <- filterLowPass -< (a1,cutoff)

noise here to simulate breath

>     breath <- noiseWhite 8 -< ()
>     outA -< (a1 + breath*0.0015) * aenv

> problem_56 = outFile "test.wav" 3 (clarinet 3 (absPitch (C,5)) 100 [])

