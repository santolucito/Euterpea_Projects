> module Main where
> import Euterpea
> import Euterpea.ExperimentalPlay
> import ImportHelp
> import System.IO.Unsafe

> import Codec.Midi


> defaultContext = Context {cTime = 0,
>                      cPlayer = fancyPlayer,
>                      cInst = Marimba,
>                      cDur = 1.0,
>                      cPch = 0,
>                      cKey = (C,Major),
>                      cVol = 100}

 readMidi :: FilePath -> [Music (Pitch,Volume)]

> readMidi fp =
>   let m = unsafePerformIO $ importFile fp
>   in
>     case m of
>       Right x ->  (eventsToMusic . midiToEvents) x
>       Left err ->  error err

> lTolist                    :: Music a -> [Music a]
> lTolist (Prim (Rest 0))    = []
> lTolist (n :+: ns)         = n : lTolist ns
> lTolist (n :=: ns)         = n : lTolist ns
> lTolist (n)                = [n]

> runme x = do
>   let t1 = head $ readMidi x
>       t2 = last $ readMidi x
>       m = (t1 :=: (line . reverse . lTolist) t2)
>   print $ dur t1
>   print $ dur t2

I think importFile from Codec.Midi needs everthing to be on a single track, multichannels are allloed

> fst3 (a,b,c)=a

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
