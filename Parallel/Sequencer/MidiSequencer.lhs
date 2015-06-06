> {-# LANGUAGE Arrows #-}

> module MidiSequencer where

> import Euterpea
> import Euterpea.ExperimentalPlay
> import Euterpea.IO.MIDI.MidiIO
> import Control.Monad
> import Control.Applicative

> import Control.Concurrent.STM
> import System.IO.Unsafe

--------
sound


> readT :: TVar a -> a
> readT x = unsafeDupablePerformIO $ atomically $ readTVar x

> breakSound :: TVar ([[Int]]) -> IO()
> breakSound v =
>   playC 
>     (defParams )
>     $ Modify (Instrument (Trumpet)) $ line $ foo v

this needs to replace the default selection (from OS) in the play function
only works on mac right now

> devI = do
>   devs <- getAllDevices
>   let d = fst $ head $ snd (devs)
>   return d

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
>   let
>     pitches = cycle [(1/8)] <**> [(a 5),(g 5),(e 5),(d 5),(c 5)]
>     makeLine uiData ps =
>       line $ zipWith (\v p -> addVolume (127*v) p) uiData (replicate 8 ps)
>     xs = zipWith makeLine (readT v) pitches
>   in
>     chord xs :  foo v
