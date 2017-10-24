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
> editor = setLayout (makeLayout (Stretchy 150) (Fixed 100)) $ 
>                title "Saving Text" $ topDown $ proc _ -> do
>   rec
>     generatedCode <- delay Nothing -< generatedCode'
>     pbeMode <- checkbox "PBE" False -< ()
>     code <- leftRight $ label "Code: " >>> textField NoWrap "2+?" -< if pbeMode then generatedCode else Nothing
>     input <-leftRight $ label "Input: " >>> textField NoWrap "1\n2" -< Nothing
>     c <- arr parse >>> unique -< (uitextToString code, uitextToString input)
>     output <- uisfPipeE (mapM runCode) -< c
>     o <- (evMap $ arr collect )>>> hold ["none"] -< output
>     modifiedOutput <- leftRight $ label "Output: " >>> textField NoWrap "" -< if pbeMode then Nothing else Just $ concat $ intersperse "\n"  o
>     generatedCode' <- arr genCode -< (uitextToString code, uitextToString input, uitextToString modifiedOutput)
>   returnA -< ()

> genCode :: (String,String,String) -> Maybe String
> genCode (code,inputExs,outputExs) = 
>   let
>     codeCandidates = permuteCode code
>     updatedCode = dropWhile (\c-> not $ codeFits (c,inputExs,outputExs)) ([code]++codeCandidates)
>   in 
>     if length updatedCode > 0 
>     then Just $ head (updatedCode)
>     else Just code

> permuteCode :: String -> [String]
> permuteCode original =
>   map (\newOp -> (map (\c -> if isOperator c then newOp else c) ) original) ['+','*','-'] ++
>   map (\i -> (map (\c -> if isNumber c then intToDigit i else c) ) original) [1..9]

> isOperator c = 
>   c=='+' || c=='*' || c=='-'

> codeFits :: (String,String,String) -> Bool
> codeFits (code,inputExs,outputExs) = let
>   o = collect $ map (unsafePerformIO. runCode) $ parse (code,inputExs)
>  in
>   outputExs == (concat $ intersperse "\n" o)

> runCode :: String -> IO (Either InterpreterError String)
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


