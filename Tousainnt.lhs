
the R_clock type will represent the
rhythm as translated to a circle

> data R_clock = R_clock {size :: Int,
>                         ticks :: [Int]}
>    deriving Show


we will have a function to create the clock
using 0,1 for now instead of Boolean
actualy maybe music is the final input here

> build_clock :: [Int] -> R_clock
> build_clock l =
>    R_clock (length l) l


"here we examine the shape of the spectrum of the
frequencies with which all the inter-onset durations
are present in a rhythm"



> has_t :: (Int,Int) -> [(Int,Int)] -> Int -> Bool
> has_t (i,v) l d = v==1 &&
>                   snd (l!!((i+d) `mod` (length l)))==1

> i_vec :: [(Int,Int)] -> Int -> Int
> i_vec l d = foldl (\ctr x -> if (has_t x l d) then (ctr+1) else (ctr+0)) 0 l

> interval_vector :: R_clock -> [Int]
> interval_vector r =
>    let l = zip [0,1..] (ticks r)
>    in
>        map (i_vec l) [1..8]

> main =
>   let
>     shiko   = build_clock [1,0,0,0,1,0,1,0,0,0,1,0,1,0,0,0]
>     son     = build_clock [1,0,0,1,0,0,1,0,0,0,1,0,1,0,0,0]
>     soukous = build_clock [1,0,0,1,0,0,1,0,0,0,1,1,0,0,0,0]
>     rumba   = build_clock [1,0,0,1,0,0,0,1,0,0,1,0,1,0,0,0]
>     bossa   = build_clock [1,0,0,1,0,0,1,0,0,0,1,0,0,1,0,0]
>     gahu    = build_clock [1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0]
>   in
>     do
>       putStrLn $ "son  " ++ (show $ interval_vector son)
>       putStrLn $ "gahu " ++ (show $ interval_vector gahu)

this geometric property could provide a heuristic for the discovery and
automatic generation of other .good. rhythms

