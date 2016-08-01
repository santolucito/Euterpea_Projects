{-# LANGUAGE ForeignFunctionInterface #-}

module FFI where

import Data.Char
import Foreign.C.Types

foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
foreign import ccall unsafe "conio.h kbhit"
  c_kbhit :: IO CInt

getHiddenChar :: IO Char
getHiddenChar = fmap (chr.fromEnum) c_getch

pullOneCharInput :: IO (Maybe Char)
pullOneCharInput = do
  h <- fmap (==0) c_kbhit
  if not h
    then sequence $ Just getHiddenChar
    else return Nothing
