> {-# LANGUAGE Arrows #-}

> module Main where
> import Euterpea
> import Euterpea.ExperimentalPlay
> import Euterpea.IO.MIDI.MidiIO
> import Control.Monad

> import Control.Concurrent.STM
> import System.IO.Unsafe
> import Control.Concurrent
> import FRP.UISF.AuxFunctions
> import Control.Applicative

> --import ElereaSequencer
> import UISFSequencer

> main :: IO ()
> main = do
>  v <- atomically $ newTVar ([[],[],[]])
>  setNumCapabilities 2
>  forkOn 2 $ breakSound v
>  game v
>  return ()

--------
sound

> readT :: TVar a -> a
> readT x = unsafeDupablePerformIO $ atomically $ readTVar x

this needs to replace the default selection (from OS) in the play function

> devI = do
>   devs <- getAllDevices
>   let d = head $ snd (devs)
>   return d

 let d' = head $ filter (\(i,d) -> name d /= "CoreMIDI") $ snd (devs)
 test = filter (\(i,d) -> name d /= "Microsoft MIDI Mapper") getAllDevices

> breakSound :: TVar ([[Int]]) -> IO()
> breakSound v =

>   playC 
>     (defParams {devID=Just (unsafeOutputID 4)})
>  --play'
>     $ Modify (Instrument (toEnum 121)) $ line $ foo v

this works because of lazy eval
we won't calculate the music value until we need to actually play it
hence we have realtime composition
For some reason, using a rest breaks make it stop playing after playing one rest

the number of tracks and number of beats is hard coded
  in both the music side and visual side.
This is actually a good thing (I think)
because we are going try to keep sound and visual seperate

> foo :: TVar ([[Int]]) -> [Music (Pitch, Volume)]
> foo v =
>    let
>      mNotes = cycle [(1/8)] <**> [(a 5),(g 5),(e 5),(d 5),(c 5)]
>      makeLine marks n =
>           line $ zipWith (\x y -> addVolume (127*x) y) marks (replicate 8 n)
>      xs = zipWith makeLine (readT v) mNotes
>    in
>      chord xs :  foo v
