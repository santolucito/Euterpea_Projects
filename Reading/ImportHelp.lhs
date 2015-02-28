> {-# LANGUAGE LambdaCase #-}

> module ImportHelp where
> import Euterpea.Music.Note.Music
> import Euterpea.Music.Note.MoreMusic
> import Euterpea.Music.Note.Performance
> import Euterpea.IO.MIDI.ToMidi
> import Euterpea.IO.MIDI.GeneralMidi
> import Data.List
> import Codec.Midi
> import System.IO.Unsafe

> import Debug.Trace

Originally from donya, reworked partially by mark

> readMidi :: FilePath -> [Music (Pitch,Volume)]
> readMidi fp = 
>   let
>     m = unsafePerformIO $ importFile fp
>   in
>     case m of
>       Right x -> cleanup $ (eventsToMusic . midiToEvents) x
>       Left err ->  error err

> readMidi' :: FilePath -> IO([Music (Pitch,Volume)])
> readMidi' fp = do
>   importFile fp >>= \case 
>     Right x ->  return $ cleanup $ (eventsToMusic . midiToEvents) x
>     Left err ->  error err

> cleanup = map (removeZeros )

> fixNegRests' :: Show a => Music a -> Music a
> fixNegRests' m = 
>  let
>    l = concat $ map ml2l $ lineToList m 
>    foo [] = Prim (Rest 0)
>    foo [x] = x
>    foo (x:x':xs) = 
>      if (dur x' < 0)
>      then x :=: foo xs
>      else x :+: foo (x':xs)
>  in 
>    foo l 

> ml2l                    :: Music a -> [Music a]
> ml2l (Prim (Rest 0))    = []
> ml2l (n :+: ns)         = n : ml2l ns
> ml2l (n :=: n')         = ml2l n ++ ml2l n'
> ml2l n                  = [n]


> fixNegRests :: Music a -> Music a
> fixNegRests (Prim p)      = Prim p
> fixNegRests (m1 :+: m2)   = 
>  let
>    m'1 = fixNegRests m1
>    m'2 = fixNegRests m2
>  in case (m'1,m'2) of
>       ((Prim (Rest x)),m)  -> 
>         if x < 0 
>         then Prim (Rest (x)) :=: m
>         else (Prim (Rest x)) :+: m
>       (m,m')  ->
>         case m' of
>           (Prim (Rest x) :+: n) ->
>             if x < 0 
>             then m :=: Prim (Rest (x)) 
>             else m :+: (Prim (Rest x))
>           _ -> m :+: m'
> fixNegRests (m1 :=: m2)   = (fixNegRests m1 :=: fixNegRests m2)
> fixNegRests (Modify c m)  = Modify c (fixNegRests m)





> data NEvent = On | Off
>   deriving (Eq, Show, Ord)
> data SimpleMsg = SE (Rational, AbsPitch, Volume, Int, NEvent) |
>               T (Rational, Rational)
>   deriving (Eq, Show)
> instance Ord (SimpleMsg) where
>     compare (SE(t,p,v,i,e)) (SE(t',p',v',i',e')) =
>         if t<t' then LT else if t>t' then GT else EQ
>     compare (T(t,x)) (SE(t',p',v',i',e')) =
>         if t<t' then LT else if t>t' then GT else EQ
>     compare (SE(t,p,v,i,e)) (T(t',x)) =
>         if t<t' then LT else if t>t' then GT else EQ
>     compare (T(t,x)) (T(t',x')) =
>         if t<t' then LT else if t>t' then GT else EQ

The importFile function places track ticks (Ticks) in a format where each value attached to a message represents the number of ticks that have passed SINCE THE LAST MESSAGE. The following function will convert input in that format into a list of pairs where the ticks are absolute. In otherwords, ticks in the output will represent the exact point in time of an event. This means that unsupported events (e.g. pitch bend) can later be filtered out without affecting the timing of support events.

> addTrackTicks :: Int -> [(Ticks, a)] -> [(Ticks, a)]
> addTrackTicks sum [] = []
> addTrackTicks sum ((t,x):ts) = (t+sum,x) : addTrackTicks (t+sum) ts

The following function addresses a ticks to Music duration conversion.

> applyTD :: TimeDiv -> SimpleMsg -> SimpleMsg
> applyTD tdw x =
>     case x of T(t,i) -> T(fixT tdw t, i)
>               SE(t,p,v,i,e) -> SE(fixT tdw t, p, v, i, e) where
> fixT tdw t =
>     case tdw of TicksPerBeat td -> t / (fromIntegral td * 4)
>                 TicksPerSecond fps tpf -> t / fromIntegral (fps * tpf)

The midiToEvents function will take a Midi structure (from importFile, for example) and convert it to a list of lists of SimpleMsgs. Each outer list represents a track in the original Midi.

> midiToEvents :: Midi -> [[SimpleMsg]]
> midiToEvents m =
>     let ts = map (simplifyTrack 0) $ map (addTrackTicks 0) (tracks m)
>     in  distributeTempos $ map (map (applyTD $ timeDiv m)) ts where
>   simplifyTrack :: Int -> [(Ticks, Message)] -> [SimpleMsg]
>   simplifyTrack icur [] = []
>   simplifyTrack icur ((t,m):ts) =
>     case m of (NoteOn c p v) ->
>                   SE (fromIntegral t, p, v, icur, On) : simplifyTrack icur ts
>               (NoteOff c p v) ->
>                   SE (fromIntegral t, p, v, icur, Off) : simplifyTrack icur ts
>               (ProgramChange c p) -> simplifyTrack (if c==9 then (-1) else p) ts
>               (TempoChange x) -> T (fromIntegral t, fromIntegral x) : simplifyTrack icur ts
>               _ -> simplifyTrack icur ts

The first track is the tempo track. It's events need to be distributed across the other tracks. This function below is called for that purpose in midiToEvents above.

> distributeTempos :: [[SimpleMsg]] -> [[SimpleMsg]]
> distributeTempos tracks =
>     if length tracks > 1 then map (sort . (head tracks ++)) (tail tracks)
>     else tracks -- importing a single-track file with embedded tempo changes.


> eventsToMusic :: [[SimpleMsg]] -> [Music (Pitch, Volume)]
> eventsToMusic tracks =
>     let tracks' = splitByInstruments tracks -- handle any mid-track program changes
>         is = map toInstr $ map getInstrument $ filter (not.null) tracks' -- instruments
>         tDef = 500000 -- current tempo, 120bpm as microseconds per qn
>     in zipWith instrument is $ map fixNegRests' $ map (seToMusic tDef 0) tracks' where
>
>   toInstr :: Int -> InstrumentName
>   toInstr i = if i<0 then Percussion else toEnum i
>

 probably best to pair up simpleMsgs then make notes, for now we will keep track of the last note off
and add that must rest. should get rest 0 for sequenctial notes(a line)

>   seToMusic :: Rational -> Rational -> [SimpleMsg] -> Music (Pitch, Volume)
>   seToMusic tCurr t3 [] = rest 0
>   seToMusic tCurr t3 (e1@(SE(t,p,v,ins,On)):es) =
>     let piMatch (SE(t1,p1,v1,ins1,e1)) = (p1==p && ins1==ins) && e1==Off
>         piMatch (T(t1,x)) = False
>         is = findIndices piMatch es -- find mactching note-offs
>         SE(t1,p1,v1,ins1, e) = es !! (is !! 0) -- pick the first matching note-off
>         r = rest (t-t3) -- create a rest since the last note off
>         n = if (dur r <0)
>             then r :+: (note (t1-t) (pitch p,v)) --if we have negative rest, play with previous note (TODO fix)
>             else r :+: (note (t1-t) (pitch p,v))
>     in  if v > 0 then -- a zero volume note is silence
>              if length is > 0 then n :+: seToMusic tCurr t1 es -- found an off
>              else seToMusic tCurr t1 ((e1:es)++[correctOff e1 es]) -- missing off case
>         else seToMusic tCurr t1 es
>   seToMusic tCurr t3 (e1@(T (t,newTempo)):es) =
>     let t2 = getTime $ head es -- find time of next event after tempo change
>         tfact = tCurr / newTempo -- calculate tempo change factor
>         es' = map (changeTime (subtract t)) es -- adjust start times
>-- for now, no tempo changes
>--         m =  tempo tfact (seToMusic newTempo t3 es')
>         m =  seToMusic newTempo t3 es
>     in if (null es || tfact == tCurr)
>        then rest 0
>        else m where
>          changeTime f (SE (t,p,v,i,e)) = SE (f t,p,v,i,e)
>          changeTime f (T (t,x)) = T (f t, x)
>   seToMusic tCurr t3 (_:es) = seToMusic tCurr t3 es -- ignore note-offs (already handled)

Finding the time of an event.

> getTime (SE(t,p,v,i,e)) = t
> getTime (T (t,x)) = t

Finding the instrument associated with a track. Only the first instrument label to appear is chosen. If a program change happens mid-track, it will not be counted.

> getInstrument ((SE(t,p,v,i,e)):xs) = i
> getInstrument ((T x) : xs) = getInstrument xs
> getInstrument [] = -1 -- No instrument assigned

The following function ensure that only one instrument appears in each list of SimpleMsgs. This is necessary in order to ensure that instrument assignments occur at the outermost level of the Music.

> splitByInstruments :: [[SimpleMsg]] -> [[SimpleMsg]]
> splitByInstruments [] = []
> splitByInstruments (t:ts) =
>     let i = getInstrument t
>         (t',t'') = splitByI i t
>         ts' = if or $ map isSE t'' then splitByInstruments (t'':ts)
>               else splitByInstruments ts
>     in  if or $ map isSE t' then t' : ts' else ts'
> isSE :: SimpleMsg -> Bool
> isSE (SE xs) = True
> isSE (T i) = False

The splitByI function partitions a stream to select a specific instrument's events.

> splitByI :: Int -> [SimpleMsg] -> ([SimpleMsg],[SimpleMsg])
> splitByI i0 [] = ([],[])
> splitByI i0 (x:xs) =
>     let (ts,fs) = splitByI i0 xs
>         f (SE(_,_,_,i1,_)) = i0 == i1
>         f _ = False
>     in  case x of SE x' -> if f x then (x:ts,fs) else (ts,x:fs)
>                   T i -> (x:ts, x:fs) -- add tempos to both streams

This function is an error-handling method for MIDI files which have mismatched note on/off events. This seems to be common in output from some software. The solution used here is to assume that the note lasts until the the time of the last event in the list.

> correctOff (SE(t,p,v,ins,e)) [] = SE(t,p,v,ins,Off)
> correctOff (SE(t,p,v,ins,e)) es =
>     let SE(t1,p1,v1,ins1,e1) = last $ filter isSE es
>     in  SE(t1,p,v,ins,Off)

