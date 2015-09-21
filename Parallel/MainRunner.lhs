> module Main where


> import Breakout
> import GameSound2
> import MediaModules

> --import Euterpea 
> --import Euterpea.Experimental
> --import Euterpea.IO.MIDI.MidiIO hiding (Time)
> --import FRP.UISF.UISF hiding (Time)
> import System.Exit
> import System.IO
> --import Control.Exception
> --import Data.List
> --import MediaModules
> import System.Environment

> import Euterpea.IO.MIDI.MidiIO

> m1 = MediaModule1 stopCo stopOp readFun

> m2 = MediaModule3 game

> main = do
>   hSetBuffering stdout NoBuffering
>   hSetBuffering stdin NoBuffering
>   hFlush stdout
>   initializeMidi
>   sendMidiOut
>   wait 2
>   openChannel 2 (-1,-1) [m1,m2]