> module HW3 where
>
> import Euterpea

Exercise 6.3 

> palin :: Music Pitch -> Bool
> palin m = m == retro m

Exercise 6.5

> test1 = (note hn (C,4)) /=: (note qn (C,4))
> test2 = repeatM (note hn (C,4)) /=: (note qn (C,4))
> test3 = (note hn (C,4)) /=: repeatM (note qn (C,4))

Exercise 6.8

> myBeat
>  = let p1 = perc LowTom qn
>        p11 = perc LowTom en
>        p2 = perc AcousticSnare qn
>        p22 = perc AcousticSnare en
>        p3 = perc SideStick sn
>     in tempo 3 $ instrument Percussion $ takeM 32 $ repeatM
>       (p1 :+: p2 :+: p11 :+: p11 :+: p22 :+: p3 :+: p3 :+:
>        p1 :+: p2 :+: p11 :+: p11 :+: p3 :+: p3 :+: p3 :+: p3)

> myMel
>  = let sa = c 5 qn
>        ri = cs 5 qn
>        ma = f 5 qn
>        pa = g 5 qn 
>        ni = as 5 qn
>        sa2 = c 6 qn
>    in tempo 3 $ instrument Sitar $ repeatM
>     (sa :+: ri :+: ma :+: pa :+: ni :+: pa :+: ni :+: sa2 :+:
>      ni :+: pa :+: ma :+: ri :+: ma :+: ri :+: sa :+: ri)

> myDrone = tempo 3 $ instrument EnglishHorn $ repeatM (c 3 bn :=: g 3 bn)

> mySong = (addVolume 100 myMel) /=: (addVolume 127 myBeat) /=: (addVolume 65 myDrone)

Exercise 6.9

> scaleVolume :: Rational -> Music (Pitch, Volume)
>                         -> Music (Pitch, Volume)
> scaleVolume s m = mMap (\(p,v) -> (p,round (s * fromIntegral v))) m

Exercise 6.13

I want a mMap that operates on the Note level, rather than the Pitch level.

> pMap2 :: (Primitive a -> Music b) -> Primitive a -> Music b
> pMap2 f (Note d x) = f (Note d x)
> pMap2 f (Rest d) = Prim(Rest d)

> mMap2 :: (Primitive a -> Music b) -> Music a -> Music b
> mMap2 f (Prim p) = pMap2 f p
> mMap2 f (m1 :+: m2 ) = mMap2 f m1 :+: mMap2 f m2
> mMap2 f (m1 :=: m2) = mMap2 f m1 :=: mMap2 f m2
> mMap2 f (Modify c m) = Modify c (mMap2 f m)


will turn a melody into an infinite melody that continues to rise
must give a melody that spans exactly one octave

> shepardTonify :: Music Pitch -> Music (Pitch,Volume)
> shepardTonify m = let harmo (Note d p) = note d (trans (-36) p):=: note d (trans (-24) p):=: note d (trans (-12) p) :=: 
>                                          note d p :=:
>                                          note d (trans (12) p) :=: note d (trans (24) p) :=: note d (trans (36) p)
>                   in mMap (\p -> (p, round( (fromIntegral (absPitch (Fs,4) - absPitch p)^(2)) ))) (mMap2 (harmo) m)

> myRow = line(map (note qn) (map pitch [60..71]))
> myRow2 = c 4 en :+: rest en :+: e 4 en :+: rest en :+: d 4 en :+: rest en :+: fs 4 en :+: rest en :+: e 4 en :+: rest en :+: gs 4 en :+: rest en :+: fs 4 en :+: rest en :+: as 4 en :+: rest en :+: gs 4 en :+: rest en  
> myShep = tempo 2 (takeM 32 (repeatM (shepardTonify (myRow))))

Exercise 7.3

> class Temporal a where
>   durT :: Temporal a => a -> Dur
>   takeT :: Temporal a => Dur -> a -> a
>   dropT :: Temporal a => Dur -> a -> a

> instance Temporal (Music m) where
>   durT m = dur m
>   takeT i m = takeM i m
>   dropT i m = dropM i m

> instance Temporal (Primitive m) where
>   durT (Note d p) = d
>   durT (Rest d) = d
>   takeT i (Note d p) = (Note i p)
>   takeT i (Rest d) = (Rest i)
>   dropT i (Note d p) = (Note (d-i) p)
>   dropT i (Rest d) = (Rest i)

Exercise 7.4

> myCompare [] (y:ys) = False
> myCompare (x:xs) [] = False
> myCompare [] [] = True
> myCompare (x:xs) (y:ys) = x == y && myCompare xs ys

> instance (Enum a, Bounded a, Eq b) => Eq (a -> b) where
>   f1 == f2 = myCompare (map f1 [minBound..maxBound]) (map f2 [minBound..maxBound])

> instance Bounded PitchClass where
>   minBound = Cff
>   maxBound = Bss

> testf1 :: PitchClass -> Int
> testf1 p = 1

> testf2 :: PitchClass -> Int
> testf2 p = 1

> testf3 :: PitchClass -> Int
> testf3 p = 2





