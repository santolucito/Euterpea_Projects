-- | Freenect examples.

module Main
  (main)
  where

--import Freenect
import Text.Printf
import Control.Monad
import Control.Monad.Fix
import Data.IORef

-- | Demos some Freenect use.
main :: IO ()
main =
      fix $ \repeat -> do
        printf "hi" 
        if False
           then return ()
           else repeat
