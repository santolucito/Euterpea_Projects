> module HW2 where
>
> import Euterpea

Exercise 3.1
1.

> f1 :: Int -> [Pitch] -> [Pitch]
> f1 s ps = map (trans s) ps

2.

> f2 :: [Dur] -> [Music a]
> f2 ds = map rest ds

3.

> f3 :: [Music Pitch] -> [Music Pitch]
> f3 ps = let f (Prim (Note d (p,o))) = note (d/2) (p,o) :+: rest (d/2)
>	      in map f ps


Exercise 3.3

ys is a list of functions, neat

> xs = [1, 2, 3] :: [Integer ]
> ys = map (+) xs :: [Integer -> Integer]

Exercise 3.4

TODO: this could use fold somehow...

> applyEach :: ([a -> a]) -> a -> [a]
> applyEach [] _ = []
> applyEach (f:fs) x = f x : applyEach fs x

> applyEach2 fs x = map ($ x) fs


Exercise 3.7

> length1 :: [a] -> Integer
> length1 xs = let add x y = 1 + y
>              in foldr (add) 0 xs

Exercise 3.9

> fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
> fuse (d:ds) (m:ms) = (m d) : fuse ds ms
> fuse [] [] = []
> fuse _ [] = error "List of Dur too long"
> fuse [] _ = error "List of Music too long"

Exercise 3.14

> frere :: Music Pitch
> frere = p1 :+: p2 :+: p3 :+: p4
> twice x = x :+: x 
> p1 = twice (c 4 qn :+: d 4 qn :+: e 4 qn :+: c 4 qn)
> p2 = twice (e 4 qn :+: f 4 qn :+: g 4 hn)
> p3 = twice (g 4 en :+: a 4 en :+: g 4 en :+: f 4 en :+: e 4 qn :+: c 4 qn)
> p4 = twice (c 4 qn :+: g 3 qn :+: c 4 hn)

this has to exist already, but its easier to rewrite than find

> multDur :: Dur -> Integer -> Dur
> multDur d 1 = 0
> multDur d i = d + multDur d (i-1)

For that matter this probably exists too
Takes a Duration between voices # of voices and music

> round1 :: Dur -> Integer -> Music Pitch -> Music Pitch
> round1 d 1 m = m 
> round1 d v m = (rest (multDur d (v-1)) :+: m) :=: (round1 d (v-1) m)

This one will give back the round in a list so you 
can further manipulate individual voices

> round2 :: Dur -> Integer -> Music Pitch -> [Music Pitch]
> round2 d 1 m = [m]
> round2 d v m = (rest (multDur d (v-1)) :+: m) : (round2 d (v-1) m)

Now we can easily change the instrument of each voice

> assignInst :: [InstrumentName] -> [Music Pitch] -> [Music Pitch]
> assignInst (i:is) (m:ms) = instrument i m : assignInst is ms
> assignInst [] [] = []
> assignInst _ [] = error "too many instruments"
> assignInst [] _ = error "not enuf instruments"

> brassQuartet = [Tuba,Trombone,FrenchHorn,Trumpet]
> frereR = round2 bn 4 frere
> frereRI = assignInst brassQuartet frereR
> solution314 = play (foldl (:=:) (rest qn) frereRI) 

Exercise 5.1

> twice1 :: (a -> a) -> a -> a
> twice1 f a = f (f a)


twice applies a function to an input, then applies that function the output of the first application
Consider 'twice twice twice' vs twice (twice twice)

Exercise 5.4





