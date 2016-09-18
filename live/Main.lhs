> {-# LANGUAGE Arrows #-}


> module Main where
> import System.Eval.Haskell
> import Data.Char
> import Data.List.Split
> import System.IO.Unsafe

> import Language.Haskell.Interpreter

> import FRP.UISF  hiding (displayStr)
> import FRP.UISF.UISF
> import FRP.UISF.Graphics.Text

-- import UiExtras

> import Data.List.Utils
> import Data.List

> main :: IO ()
> main = do
>  runUI (defaultUIParams {uiSize=(500, 500)}) editor




> editor :: UISF () ()
<<<<<<< HEAD
> editor =  title "Saving Text" $ topDown $ proc _ -> do
>   code <- leftRight $ label "Code: " >>> textField NoWrap "1+?" -< Nothing
>   input <- leftRight $ label "Input: " >>> (setLayout (makeLayout (Stretchy 300) (Fixed 300)) $ textField NoWrap "1\n2" ) -< Nothing
>   c <- arr parse >>> unique -< (code, input)
=======
> editor = setLayout (makeLayout (Stretchy 150) (Fixed 100)) $ 
>                title "Saving Text" $ topDown $ proc _ -> do
>   code <- leftRight $ label "Code: " >>> textField NoWrap "1+?" -< Nothing
>   input <-leftRight $ label "Input: " >>> textField NoWrap "1\n2" -< Nothing
>   c <- arr parse >>> unique -< (uitextToString code, uitextToString input)
>>>>>>> 9cc965b48137dcffc5cdc5fb1daaee898b584f88
>   output <- uisfPipeE (mapM runCode) -< c
>   o <- (evMap $ arr collect )>>> hold ["none"] -< output
>   leftRight $ label "Output: " >>> textField NoWrap "" -< Just $ concat $ intersperse "\n"  o
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

