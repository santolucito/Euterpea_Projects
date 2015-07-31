> {-# LANGUAGE Arrows #-}


> module Main where
> import System.Eval.Haskell
> import Data.Char
> import Data.List.Split
> import System.IO.Unsafe

> import Language.Haskell.Interpreter

> import FRP.UISF  hiding (displayStr)
> import FRP.UISF.UISF

-- import UiExtras

> import Data.List.Utils
> import Data.List

> main :: IO ()
> main = do
>  runUI (defaultUIParams {uiSize=(500, 500)}) editor




> editor :: UISF () ()
> editor = setLayout (makeLayout (Stretchy 150) (Fixed 100)) $ 
>                title "Saving Text" $ topDown $ proc _ -> do
>   code <- leftRight $ label "Code: " >>> textField 5 "1+?" -< Nothing
>   input <-leftRight $ label "Input: " >>> textField 5 "1\n2" -< Nothing
>   c <- arr parse >>> unique -< (code, input)
>   output <- uisfPipeE (mapM runCode) -< c
>   o <- (evMap $ arr collect )>>> hold ["none"] -< output
>   leftRight $ label "Output: " >>> textField 5 "" -< Just $ concat $ intersperse "\n"  o
>   returnA -< ()


> runCode c = 
>   runInterpreter $ setImports ["Prelude"] >> interpret c (as::String)

> collect :: [Either InterpreterError String] -> [String]
> collect (x:xs) =
>   let
>     f x = case x of
>       Left x -> show x 
>       Right x -> x
>   in
>     f x : collect xs
> collect [] = []

> parse :: (String,String) -> [String]
> parse (c,i) = 
>   let
>     is = splitOn ['\n'] i :: [String]
>     all_c = map (replace_line c) is :: [String]
>   in
>     map (\x -> "show (" ++ x ++ ")") all_c


> replace_line :: String -> String -> String
> replace_line code inp = replace "?" inp code 


use existenial types for (as::...) to let us write code for more types
doesn't work since we interpreter works on a very low level


