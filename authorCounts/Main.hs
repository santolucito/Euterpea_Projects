{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.List
import Control.Monad

verbose = False

main = do
  putStrLn "Data collected from http://www.cs.utexas.edu/users/hunt/FMCAD/FMCAD15/accepted.shtml"
  calcScore "FMCAD15"
  putStrLn "Data collected from http://www.cs.utexas.edu/users/hunt/FMCAD/FMCAD16/accepted.html"
  calcScore "FMCAD16"
  putStrLn "Data collected from http://i-cav.org/2015/papers/"
  calcScore "CAV15"
  putStrLn "Data collected from http://i-cav.org/2016/accepted-papers/"
  calcScore "CAV16"

calcScore :: String -> IO()
calcScore file = do
  f <- readFile file
  let ns = process f
  when verbose $putStrLn $ file++" papers with >=4 authors"
  when verbose $ print ns
  when verbose $ putStrLn ""
  when verbose $ putStrLn $ file++" papers with >=4 authors and non-alphabetical order"
  let x = filter (\n -> n /= sort n) ns
  when verbose $ print x
  putStrLn ""
  putStrLn $ "Percentage of "++file++" paper with >=4 authors and non-alphabetical order"
  print $ 100*((fromIntegral $ length x) / (fromIntegral $length ns) )
  print ((show $ length x ) ++ " / " ++ (show $length ns))
  putStrLn ""

--process :: String -> Int
process f = let
  ls = map T.pack $ lines f
  as = map (fst.T.breakOn "#") ls
  ns = map lastNameList as
  ns' = filter (\n -> length n >= 4) ns
 in
  ns'

lastNameList :: T.Text -> [T.Text]
lastNameList t = let
  t' = T.replace " and " "," t
  as = T.splitOn "," t'
  ls = filter (/="") $ map (T.strip.last.T.splitOn " ".T.strip) as
 in
  ls
  
