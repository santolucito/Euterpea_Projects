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

We have three proposals

1 - Make + work on SF

this is a really bad version of just making it an instance
(~+) :: Num a => AudSF () a -> AudSF () a ->  AudSF () a
(~+) sf1 sf2 = proc () -> do
                x <- sf1 -< ()
                y <- sf2 -< ()
                outA -< x + y


> instance Num a => Num (AudSF () a) where
>  s1 + s2 = proc (s1,s2) -> do
>                 outA -< s1 + s2

2 - +' :: AudSF(a,a) a


 the following typechecks as is :
w' = f1 x + f2 y
