> {-# LANGUAGE Arrows #-}

> module Main where
> import Euterpea
> import Control.Arrow


Ok, we have three differnet approaches we want to try.
In the end they should all function in the same way as this

> runme = outFile "test.wav" 0.01 z

> z :: AudSF () Double
> z = proc () -> do
>    let x = 0.1
>        y = 0.1
>    u <- arr1 -< x
>    v <- arr2 -< y
>    let w = u + v
>    outA -< w

> arr1 :: AudSF Double Double
> arr1 = proc x -> do
>    a <- arr (+0.1) -< x
>    outA -< a

> arr2 :: AudSF Double Double
> arr2 = proc x -> do
>    a <- arr (+0.2) -< x
>    outA -< a

Now, our idea is to change z to look like this

z' :: AudSF () Double
z' = proc () -> do
    let x = 0.1
        y = 0.1
    let w = (arr1 -< x) ~+ (arr2 -< y)
    outA -< w

But to do that we need to define what (arr1 -< x) is and how + operates on it

we could use this to pass any arbitrary function to combine two arrows
very extensible, but doesnt look so nice,
also shouldnt need to pair up the input

> combine :: (b -> d -> e) -> AudSF a b -> AudSF c d ->  AudSF (a,c) e
> combine f sf1 sf2 = proc (a,c) -> do
>     sf1Val <- sf1 -< a
>     sf2Val <- sf2 -< c
>     returnA -< f sf1Val sf2Val

> z' :: AudSF () Double
> z' =
>   let
>       w = combine (+) arr1 arr2
>       v = combine (*) w arr1
>   in
>     proc () -> do
>      let x = 0.1
>          y = 0.1
>      o <- v -< ((x,y),y)
>      outA -< o

or more elegantly, make it an instance of whatever we want
im not sure this lets us use our own functions tho, maybe it does actually
can we do something like
f :: AudSF () a -> AudSF () a -> AudSF () a
f = +

instance Num a => Num (AudSF () a) where
 sf1 + sf2 = proc () -> do
      x <- sf1 -< ()
      y <- sf2 -< ()
      outA -< x + y



 the following typechecks as is :
w' = f1 x + f2 y
