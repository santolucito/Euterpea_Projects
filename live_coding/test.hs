module LiveCoding where

import Euterpea
import Euterpea.IO.MIDI.MidiIO
import Control.Concurrent
import Euterpea.IO.MUI.MidiWidgets
import Control.Monad
import Codec.Midi (Time)

setup :: IO ()
setup = do
  allDevs <- getAllDevices
  let devID = fst $ head $ snd allDevs 
  forkIO $ forever $ outputMidi devID
  return ()

getD :: IO (OutputDeviceID)
getD = do
  allDevs <- getAllDevices
  let devID = fst $ head $ snd allDevs
  return devID

playLive id m = do
  mapM_ (deliverMidiEvent id) (toMidi' m)

toNote pc = pc 4 qn
mel = line $ map toNote $ concat $ replicate 10 [c, d, e, c]
mel' = line $ map toNote $ concat $ replicate 10 [e, f, g, g]

type MidiEvent = (Codec.Midi.Time, MidiMessage)
toMidi' :: Music Pitch -> [MidiEvent]
toMidi' m = musicToMsgs False [AcousticGrandPiano] (toMusic1 m)


