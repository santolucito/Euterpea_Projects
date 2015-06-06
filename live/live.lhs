> {-# LANGUAGE Arrows #-}


> module Main where
> import FRP.UISF
> import System.Eval.Haskell
> import Data.Char
> import System.IO.Unsafe

> import Language.Haskell.Interpreter


> main :: IO ()
> main = do
>  runUI (defaultUIParams {uiSize=(500, 500)}) editor

  i <- eval "1::Int" [] :: IO (Maybe Int)
  when (isJust i) $ putStrLn (show (fromJust i))


> editor :: UISF () ()
> editor = setLayout (makeLayout (Stretchy 150) (Fixed 100)) $ 
>                title "Saving Text" $ topDown $ proc _ -> do
>   code <- leftRight $ label "Code: " >>> textboxE "2+1::Int" -< Nothing
>   output <- liftAIO (\c -> runInterpreter $ setImports ["Prelude"] >> interpret c (as::Int)) -< code
>   liftAIO (\c -> putStrLn c) -< show(code)
>   liftAIO (\c -> putStrLn c) -< show(output)
>   leftRight $ label "Output: " >>> displayStr -< show(output)
>   returnA -< ()

   output <- liftAIO (\c -> eval c [] :: IO (Maybe Int) ) -< code
   input <- leftRight $ label "Input: " >>> textboxE "" -< Nothing
   o <- arr (\x -> if (isJust x) then (show(fromJust x)) else "error") -<< output
 

  instance ArrowApply UISF where
   app  = UISF $ (f,x) ->  

