> module Main where
> import Euterpea
> import Euterpea.ExperimentalPlay
> import ImportHelp

> r x = do
>   i <- readMidi' x
>   let m = map (takeM 8) i
>       t1 = head m
>       t2 = head $ tail  m
>       m' = t1 :=: t2
>   return m'

> runme1 x= do
>   r x >>= (writeMidi "testOut.mid")

> runme2 x = do
>   r x >>= print

> runme3 x= do
>   r x >>= play


> runme x = do
>   i <- readMidi' x
>   let m = map (takeM 4) i
>       t1 = head m
>       t2 = revM $ last m
>       m' = t1 :+: t2
>   writeMidi "testOut.mid" m'


 testRun1 x = do
   let m = takeM 4 $ fst3 $ readMidi x
       m' = m :+: (revM m)
   play $ timesM 4 m'



> lTolist                    :: Music a -> [Music a]
> lTolist (Prim (Rest 0))    = []
> lTolist (n :+: ns)         = n : lTolist ns
> lTolist (n :=: ns)         = n : lTolist ns
> lTolist (n)                = [n]

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
