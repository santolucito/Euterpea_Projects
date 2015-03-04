> module Main where
> import Euterpea hiding (f)
> import Euterpea.ExperimentalPlay
> import ImportHelp

> import Data.List

> r x = do
>   i <- readMidi' x
>   let 
>     t1 = head i
>     t2 = head $ tail i
>     m  = t1 :=: t2
>   return $ f m

f :: Music Pitch -> Music Pitch

> f m =
>     line $ sortBy avgP (phrases m)

 avgP :: Music a -> Int

> avgP m m'
>    | p >  p'        =  GT
>    | p == p'        =  EQ
>    | p <  p'        =  LT
>  where
>     f = sum . map foo . lToList 
>     p = f m
>     p'= f m'

 phrases :: Music a -> [Music a]

> phrases m 
>  | dur m > 0 = takeM 4 m : (phrases $ dropM 4 m)
>  | dur m <= 0 = []

> foo :: Primitive (Pitch,Volume) -> Int
> foo (Rest _) = 0
> foo (Note d (p,_)) = absPitch p


> runme1 x= do
>   r x >>= (writeMidi "testOut.mid")

> runme2 x = do
>   r x >>= print

> runme3 x= do
>   r x >>= play

> lToList  :: Music a -> [Primitive a]
> lToList (Prim x)      = [x]
> lToList (n :+: ns)           = lToList n ++ lToList ns
> lToList (n :=: ns)           = lToList n ++ lToList ns
> lToList (Modify _ ns)  = lToList ns

> main = runme1 "TurkishMarch.mid"


I think importFile from Codec.Midi needs everthing to be on a single track, multichannels are allloed

works well with TM2.mid
need to fix play' from ExperimentalPlay

 testRun1 x = do
   let m = takeM 4 $ fst3 $ readMidi x
       m' = m :+: (revM m)
   play $ timesM 4 m'

 testRun x = print $ removeZeros $ takeM 1 $ fst3 $ readMidi x

Use UDP to connect to Max

Midi format 0 everything is on one track
Music1 is multitrack, need to split the tracks to edit normally
