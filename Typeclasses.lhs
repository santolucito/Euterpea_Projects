> module Main where
> import Control.Applicative
> import Control.Arrow

> main :: IO()
> main = do
>    putStrLn $ show f
>    putStrLn $ show af
>    putStrLn $ show m

a normal functor lets you apply normal function to values with a context

> f = fmap (+3) (Just 3)

an applicitiave functor lets you apply functions with a context to values with context

> af = Just (+3) <*> Just 3

a monad lets you apply functions that generate a context to values with a context

> m = Just 3 >>= (\x -> return $ x+3)

an arrow lets you

> a :: Arrow a => a Int Int
> a = arr (+3 )
