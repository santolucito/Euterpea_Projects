> {-# LANGUAGE Arrows #-}


> module Main where
> import FRP.UISF  
> import System.Eval.Haskell
> import Data.Char
> import Data.List.Split
> import System.IO.Unsafe

> import Language.Haskell.Interpreter

> import FRP.UISF  hiding (displayStr)

> import UiExtras
> import Data.List.Utils

> main :: IO ()
> main = do
>  runUI (defaultUIParams {uiSize=(500, 500)}) editor




> editor :: UISF () ()
> editor = setLayout (makeLayout (Stretchy 150) (Fixed 100)) $ 
>                title "Saving Text" $ topDown $ proc _ -> do
>   code <- leftRight $ label "Code: " >>> textbox2 3 "1+?" -< Nothing
>   input <- leftRight $ label "Input: " >>> textbox2 1 "1,2" -< Nothing
>   c <- arr parse -< (code,input)
>   output <- liftAIO (map runCode) -< c
>   o <- arr foo -< output
>   --leftRight $ label "Output: " >>> displayField -< o
>   leftRight $ label "Output: " >>> displayField -< o
>   returnA -< ()

  runCode :: String -> m (Either InterpreterError String)

> runCode c = 
>   runInterpreter $ setImports ["Prelude","Euterpea"] >> interpret c (as::String)

> foo x =
>   case x of
>     Left x -> show x 
>     Right x -> x


> parse :: (String,String) -> [String]
> parse (c,i) = 
>   let
>     is = splitOn "," i :: [String]
>     all_c = map (replace_line c) is :: [String]
>   in
>     map (\x -> "show (" ++ x ++ ")") all_c


> replace_line :: String -> String -> String
> replace_line code inp = replace "?" inp code 

 replace_line code inp = foldl "" (replace_char inp ++) code 

> replace_char :: String -> Char -> String
> replace_char inp code = if (code=='?') then inp else [code]

use existenial types for (as::...) to let us write code for more types
doesn't work since we interpreter works on a very low level


