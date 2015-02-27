> module Main where
> import Euterpea
> import Euterpea.ExperimentalPlay

> import System.IO.Unsafe


> defaultContext = Context {cTime = 0,
>                      cPlayer = fancyPlayer,
>                      cInst = Marimba,
>                      cDur = 1.0,
>                      cPch = 0,
>                      cKey = (C,Major),
>                      cVol = 100}

> readMidi :: FilePath -> (Music1, Context (Pitch, [NoteAttribute]), UserPatchMap)
> readMidi fp =
>   let m = unsafePerformIO $ importFile fp
>   in
>     case m of
>       Left _ -> (toMusic1 (c 4 qn),defaultContext,[(Cello,1)])
>       Right x -> fromMidi x

> fst3 (a,b,c)=a

works well with TM2.mid
need to fix play' from ExperimentalPlay

> testRun1 x = do
>   let m = takeM 4 $ fst3 $ readMidi x
>       m' = m :+: (revM m)
>   play $ timesM 4 m'

> testRun x = fst3 $ readMidi x

Use UDP to connect to Max

Music1 is multitrack, need to split the tracks to edit normally
