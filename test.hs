> {-# LANGUAGE Arrows #-}

try and play two samples
one after another

> module Main where
  > import Euterpea
  > import Euterpea.IO.Audio.PortAudioChannel
  > import Control.Arrow
  > import Control.Concurrent
  > import Control.Concurrent.STM

  STM

  > type VolChan = TChan Double

  GUI

  > volume_slider :: VolChan -> UISF () (Double)
  > volume_slider v = proc _ -> do
    >    a <- title "volume"  $ vSlider (0,1) 0 -< ()
    >    _ <- display -< 1-a
    > --   writeTChan v 1-a
    >    outA -< 1-a

    > mixer_board :: VolChan -> UISF () ()
    > mixer_board v = title "Mixer" $ proc _ -> do
      >    _ <- volume_slider v -< ()
      >    returnA -< ()

      Audio

      > volume_control :: AudSF (Double,Double) (Double)
      > volume_control = arr (\(s,v) -> (s*v))

      read_volume :: VolChan -> AudSF () (Double)
      read_volume = proc () -> do
        v <- readTChan

        > wavloop :: VolChan -> IO ()
        > wavloop v = wavSFInf "input2.wav" >>= playSignal 20


        > main :: IO ()
        > main = do
          >  v <- atomically newTChan
          >  setNumCapabilities 2
          >  forkOn 1 $ runUI "UI Demo" $ mixer_board v
          >  forkOn 2 $ wavloop v
          >  return ()


          foo :: [Int] -> Int
          foo = map

        
          use STM to write the UISF value then read in the AudSF thread
          lift monad to arrow to allow for STM to work in arrow?
          at that point i can instead just lift m to arr for runUI and playSingal
          then i can compose the two, and pass state same as uisf deos

          so i want
          Kleisli IO
          IO Monad
          UISF and AudSF

          or does kleisli IO replace IO Monad
