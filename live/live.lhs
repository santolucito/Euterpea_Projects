> {-# LANGUAGE Arrows #-}


> module Main where
> import FRP.UISF  hiding (displayStr)
> import System.Eval.Haskell
> import Data.Char
> import System.IO.Unsafe

> import Language.Haskell.Interpreter


> import UiExtras

> main :: IO ()
> main = do
>  runUI (defaultUIParams {uiSize=(500, 500)}) editor




> editor :: UISF () ()
> editor = setLayout (makeLayout (Stretchy 150) (Fixed 100)) $ 
>                title "Saving Text" $ topDown $ proc _ -> do
>   code <- leftRight $ label "Code: " >>> textbox2 3 "1::Int" -< Nothing
>   c <- arr parse -< code
>   output <- liftAIO runCode -< c
>   o <- arr foo -< output
>   leftRight $ label "Output: " >>> displayField -< o
>   returnA -< ()

  runCode :: String -> m (Either InterpreterError String)

> runCode c = 
>   runInterpreter $ setImports ["Prelude","Euterpea"] >> interpret c (as::String)

> foo x =
>   case x of
>     Left x -> show x 
>     Right x -> x


> parse :: String -> String
> parse x = 
>   "show (" ++ x ++ ")"

use existenial types for (as::...) to let us write code for more types
doesn't work since we interpreter works on a very low level


