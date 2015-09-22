> {-# LANGUAGE Arrows #-}


> module GameSound2 where
> import Euterpea
> import Euterpea.ExperimentalPlay
> import Control.Monad

> import Control.Concurrent.STM
> import System.IO.Unsafe
> import Control.Concurrent
> import FRP.UISF.AuxFunctions
> import FRP.UISF.Asynchrony
> import Control.Applicative
> import Euterpea.IO.MIDI.MidiIO hiding (Time)

> import Euterpea.ExperimentalPlay

> defaultID = 0

> stopCo :: (Double, Double) -> Bool
> stopCo (a,b) = a < -1 || b < -1

> stopOp :: (Double,Double) -> IO()
> stopOp _ = terminateMidi

> update :: (Double,Double) -> IO(Double,Double)
> update (a,b) = do
>   if a>=0 && b>=0 then putStrLn (show (a,b)) else return ()
>   return (-1,-1)

> readFun:: (Double,Double) -> IO ()
> readFun (a,b) = do
>   if a>=0 && b>=0 then (sendMidiOut $floor a) >> putStrLn "test" else return()
>   return ()

> sendMidiOut ::  Int -> IO ()
> sendMidiOut x = outputMidi (unsafeOutputID defaultID)>>
>     deliverMidiEvent (unsafeOutputID defaultID) (0, Std $ NoteOn 0 x 100)
