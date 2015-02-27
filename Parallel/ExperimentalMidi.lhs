Special playback functions
Created by Donya Quick
Last modified: 03-Dec-2014

Experimental Midi I/O implementation.

> module ExperimentalMidi where
> import Codec.Midi hiding (Tempo)
> import Control.DeepSeq
> import Control.Monad
> import Control.Concurrent
> import Control.Exception
> import Data.List
> import Euterpea.IO.MIDI.MidiIO
> import Euterpea.IO.MIDI.ToMidi
> import Euterpea.Music.Note.Music
> import Euterpea.Music.Note.Performance
> import Sound.PortMidi
> import System.IO.Unsafe (unsafePerformIO)

> import Control.Concurrent
> import Control.Concurrent.STM
> import Control.Monad
> import Euterpea.ExperimentalPlay


> sendMidiOut :: OutputDeviceID -> Maybe [(Time, MidiMessage)] -> IO ()
> sendMidiOut dev ms = outputMidi dev >> maybe (return ()) 
>                  (mapM_ (\(t,m) -> deliverMidiEvent dev (0, m))) ms

> midiInLoop :: TVar Bool -> TVar (InputDeviceID,OutputDeviceID) -> IO ()
> midiInLoop stopVar devsVar = do
>     quit <- atomically $ readTVar stopVar -- read from stopVar
>     (devIn, devOut) <- atomically $ readTVar devsVar -- read from devsVar
>     let f Nothing = Nothing
>         f (Just (t,ms)) = Just $ map (\m -> (0, Std $ m)) ms
>     msgs <- getMidiInput devIn -- get MIDI messages coming in
>     sendMidiOut devOut $ f msgs
>     if quit then return () else midiInLoop stopVar devsVar

> midiInLoopMulti :: TVar Bool -> TVar ([InputDeviceID],[OutputDeviceID]) -> IO ()
> midiInLoopMulti stopVar devsVar = do
>     quit <- atomically $ readTVar stopVar -- read from stopVar
>     (devsIn, devsOut) <- atomically $ readTVar devsVar -- read from devsVar
>     let f [] = Nothing
>         f xs = Just $ map (\m -> (0, Std $ m)) xs
>         g Nothing = []
>         g (Just (t,ms)) = ms
>     msgs <- sequence $ map getMidiInput devsIn -- get MIDI messages coming 
>     let outVal = f $ concatMap g msgs
>     sequence $ map (\d -> sendMidiOut d outVal) devsOut
>     if quit then return () else midiInLoopMulti stopVar devsVar


> midiInWrapMulti :: [Int] -> [Int] -> IO ()
> midiInWrapMulti mIns mOuts =  
>     let mIn = map unsafeInputID mIns
>         mOut = map unsafeOutputID mOuts
>     in  handleCtrlC $ do
>         s <- newTVarIO False
>         v <- newTVarIO (mIn, mOut)
>         midiInLoopMulti s v where
>         handleCtrlC :: IO a -> IO a
>         handleCtrlC op = onException op terminateMidi


> midiInWrap :: Int -> Int -> IO ()
> midiInWrap mInX mOutX = 
>     let mIn = unsafeInputID mInX
>         mOut = unsafeOutputID mOutX
>     in  handleCtrlC $ do 
>         s <- newTVarIO False
>         v <- newTVarIO (mIn, mOut)
>         midiInLoop s v where
>         handleCtrlC :: IO a -> IO a
>         handleCtrlC op = onException op terminateMidi

> getMidiInput :: InputDeviceID -> IO (Maybe (Time, [Message])) -- Codec.Midi message format
> getMidiInput dev = pollMidi dev



> midiOutOp :: (Num a, Ord a) => TVar a -> IO ()
> midiOutOp statusVar = do
>     state <- atomically $ readTVar statusVar -- read from statusVar
>     if state == 0 then midiOutOp statusVar -- keep looping
>       else if state > 0 then play' (c 4 qn) >> midiOutOp statusVar -- play something
>       else return () -- stop


> inDevices = fst $ unsafePerformIO getAllDevices
> outDevices = snd $ unsafePerformIO getAllDevices
   
> refresh = terminateMidi >> terminate