> {-# LANGUAGE Arrows #-}


> module Main where
> import FRP.UISF  
> import System.Eval.Haskell
> import Data.Char
> import Data.List.Split
> import System.IO.Unsafe

> import Language.Haskell.Interpreter

 import FRP.UISF  hiding (displayStr)

 import UiExtras

> main :: IO ()
> main = do
>  runUI (defaultUIParams {uiSize=(500, 500)}) editor




> editor :: UISF () ()
> editor = setLayout (makeLayout (Stretchy 150) (Fixed 100)) $ 
>                title "Saving Text" $ topDown $ proc _ -> do
>   code <- leftRight $ label "Code: " >>> textboxE "1+" -< Nothing
>   input <- leftRight $ label "Input: " >>> textboxE "1" -< Nothing
>   c <- arr parse -< (code,input)
>   output <- liftAIO (concat. map runCode) -< c
>   o <- arr foo -< output
>   --leftRight $ label "Output: " >>> displayField -< o
>   leftRight $ label "Output: " >>> displayStr -< o
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
>     is = splitOn "," i
>     replace_char inp code = if (code=="?") then inp else code
>     replace_line code inp = map (replace_char inp) code
>     all_c = map (replace_line c) is
>   in
>     map (\x -> "show (" ++ x ++ ")") all_c

use existenial types for (as::...) to let us write code for more types
doesn't work since we interpreter works on a very low level


