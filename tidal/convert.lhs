
> module Main where
> import Euterpea
> import Sound.Tidal.Context
> import Sound.Tidal.VolcaKeys as M


This is a module to convert Euterpea's 'Music a' to a format
compatable to tidal

> song = (c 4 qn) :+: (d 4 qn)

 euToTidal :: Music Pitch -> Pattern Int
 euToTidal = listToPat. lineToPattern. flatM



we want a function that maps the list of music values (a line)
to a thing we can send to the pattern creator.
the patern creator has a whole parser that will make rythms for us

> lineToPattern :: [(Dur, Pitch)] -> (String,[Int])
> lineToPattern l = 
>   let 
>     p = map absPitch (snd l)
>     r = genStr (fst l)
>   in
>     (r,p)


This is the complicated part. need to generate a string from a list of rhythms.
need to find if [x x] [y y] is :=: or :+:


> genStr :: [Dur] -> String
> genStr = 






take a music value and turn it into a list, only taking the top line

> flatM :: Music Pitch -> [(Dur,Pitch)]
> flatM (Prim (Note d x)) = [(d,x)]
> flatM (Prim (Rest d)) = [(d,(C,0))]
> flatM (x :+: y)= flatM x ++ flatM y
> flatM (x :=: y)= flatM x
> flatM (Modify _ x) = flatM x


