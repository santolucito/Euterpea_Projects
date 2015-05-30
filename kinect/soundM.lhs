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

> import KinectModule

> main :: IO ()
> main = do
>  v <- atomically $ newTVar (0)
>  setNumCapabilities 2
>  forkOn 2 $ makeSound v
>  kinect_start v
>  return ()


We are goning to take depth data from the kinect module and make sounds out of it

--------
sound

this needs to replace the default selection (from OS) in the play function

> devI = do
>   devs <- getAllDevices
>   let d = fst $ head $ snd (devs)
>   return d

> readT :: TVar a -> a
> readT x = unsafeDupablePerformIO $ atomically $ readTVar x

> makeSound :: TVar Int -> IO()
> makeSound v =
>   playC 
>     (defParams {devID=Just (unsafePerformIO devI)})
>     $ Modify (Instrument (Trumpet)) $ line $ foo v

this works because of lazy eval
we won't calculate the music value until we need to actually play it
hence we have realtime composition
For some reason, using a rest breaks make it stop playing after playing one rest

this will basically be random notes, but it works for now.
just use a smarter scaling than mod to get something sensical

> foo :: TVar (Int) -> [Music Pitch]
> foo dataChan =
>   let
>     inData = readT dataChan
>     currNote = note qn (pitch (30 + mod inData 30))
>   in
>     currNote :  foo dataChan
