> module HW1 where
>
> import Euterpea

Exercise 1.1
simple (simple 2 3 4) 5 6 
=> simple (2 * (3 + 4)) 5 6
=> simple (2 * 7) 5 6
=> simple 14 5 6
=> 14 * (5 + 6)
=> 151

Exercise 1.2
simple (a - b) a b
=> (a - b) * (a + b)
=> (a * (a + b)) + (-b * (a + b))
=> ((a * a) + (a * b)) + ((-b * a) + (-b * b))
=> (a * a) + (a * b) - (a * b) + (-b * b)
=> (a * a) + (-b * b)
=> a^2 - b^2

Exercise 1.3
[A,B,C] :: [PitchClass]
[D,2] is ill typed beacuse lists must contain a single type
(-42,Ef) :: [Integer, PitchClass]
[('a',3),('b',5)] :: [(Char,Integer)]
simple 'a' 'b' 'c' is ill typed because we have not described a function simple to take three chars
(simple 1 2 3, simple) :: (Integer, Integer -> Integer -> Integer -> Integer)
["I", "love", "Euterpea"] :: [[Char]]

Exercise 1.4

s is the shift in half-steps of the harmonization

> hNote :: Dur -> Int -> Pitch -> Music Pitch
> hNote d s p = note d p :=: note d (trans (s) p)

> hList :: Dur -> Int -> [Pitch] -> Music Pitch
> hList d s [] = rest 0
> hList d s (p:ps) = hNote d s p :+: hList d s ps

> mel :: Music Pitch
> mel = hList qn (-3) [(A,4),(B,4),(C,5)]

Exercise 2.1

> t251 :: Music Pitch
> t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
>            gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
>            cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
>        in dMinor :+: gMajor :+: cMajor

> minorTriad :: Dur -> Pitch -> Music Pitch
> minorTriad d p = note d p :=: hNote d 3 p :=: hNote d 7 p

> majorTriad :: Dur -> Pitch -> Music Pitch
> majorTriad d p = note d p :=: hNote d 4 p :=: hNote d 7 p

> twoFiveOne :: Pitch -> Dur -> Music Pitch
> twoFiveOne p d = let minorTwo = minorTriad d (trans 2 p) 
>                      majorFive = majorTriad d (trans 7 p) 
>                      majorOne = majorTriad (2*d) p
>                  in minorTwo :+: majorFive :+: majorOne

twoFiveOne (C,4) wn
=> let minorTwo = minorTriad wn (trans 2 (C,4)) 
       majorFive = majorTriad wn (trans 7 (C,4)) 
       majorOne = majorTriad (2*wn) (C,4)
    in minorTwo :+: majorFive :+: majorOne
=> let minorTwo = minorTriad wn (D,4) 
       majorFive = majorTriad wn (G,4)
       majorOne = majorTriad bn (C,4)
    in minorTwo :+: majorFive :+: majorOne
=> let minorTwo = note wn (D,4) :=: hNote wn 3 (D,4) :=: hNote wn 7 (D,4)
       majorFive = note wn (G,4) :=: hNote wn 4 (G,4) :=: hNote wn 7 (G,4)
       majorOne = note bn (C,4) :=: hNote bn 4 (C,4) :=: hNote bn 7 (C,4)
    in minorTwo :+: majorFive :+: majorOne
=> let minorTwo = note wn (D,4) :=: note wn(F,4) :=: note wn (A,5)
       majorFive = note wn (G,4) :=: note wn (B,5) :=: note wn (D,5)
       majorOne = note bn (C,4) :=: note bn (E,4) :=: note bn (G,4)
    in minorTwo :+: majorFive :+: majorOne  
=> t251

Exercise 2.2

> data BluesPitchClass = Ro | MT | Fo | Fi | MS
> type BluesPitch = (BluesPitchClass, Octave)
> 
> ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
> ro o d = note d (Ro, o)
> mt o d = note d (MT, o)
> fo o d = note d (Fo, o)
> fi o d = note d (Fi, o)
> ms o d = note d (MS, o)

I think we really want
fromBlues :: Pitch -> Music BluesPitch -> Music Pitch
We shouldnt be able to go from the abstract notes to
realized pitches without a target key area.
For now we map to C and must rememeber to transpose to
the inteneded key

>
> fromBlues :: Music BluesPitch -> Music Pitch
> fromBlues (Prim (Note d (p,o))) = case p of
>     Ro -> note d (C,o) 
>     MT -> note d (Ef,o)
>     Fo -> note d (F,o)
>     Fi -> note d (G,o)
>     MS -> note d (Bf,o)
>
> fromBlues (Prim (Rest d)) = rest d
> fromBlues (m1 :+: m2) = (fromBlues m1 :+: fromBlues m2)
> fromBlues (m1 :=: m2) = (fromBlues m1 :=: fromBlues m2)
> fromBlues (Modify ctrl m1) = Modify ctrl (fromBlues m1)

> mel1 :: Music BluesPitch
> mel1 = ro 4 qn :+: fo 4 qn :+: mt 4 qn :+: fi 4 qn :+:
>        ms 4 qn :+: ro 5 qn
> my_test1 = play (fromBlues mel1)
	
> mel2 :: Music BluesPitch
> mel2 = ro 4 qn :+: mt 4 qn :+: fo 4 en :+: fi 4 en :+: fo 4 en :+: fi 4 en :+:
>        ms 3 qn :+: ro 4 qn
> my_test2 = play (fromBlues mel2)

> mel3 :: Music BluesPitch
> mel3 = ro 5 qn :+: ms 4 qn :+: fi 4 qn :+: fo 4 en :+: mt 4 en :+:
>        ms 3 en :+: ro 4 qn
> my_test3 = play (fromBlues mel3)

Exercise 2.4
Show that trans i (trans j p) = trans (i + j ) p.
trans i (trans j p)
=> trans i (pitch (absPitch p + j))
=> pitch (absPitch (pitch (absPitch p + j)) + i)
=> pitch ((absPitch p) + j + i)
=> trans (i + j) p

Exercise 2.5

> transM :: AbsPitch -> Music Pitch -> Music Pitch
> transM ap (Prim (Note d p)) = note d (trans ap p)
> transM ap (Prim (Rest d)) = rest d
> transM ap (m1 :+: m2) = (transM ap m1) :+: (transM ap m2)
> transM ap (m1 :=: m2) = (transM ap m1) :=: (transM ap m2)
> transM ap (Modify ctrl m1) = Modify ctrl (transM ap m1)

> my_test4 = play (transM 3 (fromBlues mel1))






