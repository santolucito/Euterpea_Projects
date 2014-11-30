> {-# LANGUAGE Arrows #-}

try and play two samples
one after another

> module Main where
> import Euterpea
> import Euterpea.IO.Audio.PortAudioChannel
> import Control.Arrow
> import Control.Concurrent
> import Control.Concurrent.STM

> import System.IO.Unsafe

STM

> type VolChan = TChan Double

GUI

the problem with convertToUISF is that then you ahve to write enough samples
to fill the buffer until the next tick of the clock (uisf at 60fps, audsf at 44k)
so this is a bad idea
    _ <- convertToUISF 60 60


> volume_slider :: UISF () (Double)
> volume_slider = proc _ -> do
>    a <- title "volume"  $ vSlider (0,1) 0 -< ()
>    _ <- display -< 1-a
>    outA -< 1-a

> writeArr :: VolChan -> UISF (Double) ()
> writeArr v = arr (\x -> unsafePerformIO $ atomically $ writeTChan v x)

> mixer_board :: VolChan -> UISF () ()
> mixer_board vc = title "Mixer" $ proc _ -> do
>    v <- volume_slider -< ()
>    _ <- writeArr vc -< 1-v
>    returnA -< ()

Audio

> volume_control :: AudSF (Double, Double) (Double)
> volume_control = arr (\(s,v) -> (s* v))

> read_volume :: VolChan -> (AudSF () Double)
> read_volume v =  arr (\x -> unsafePerformIO $ atomically $ peekTChan v)

 read_volume :: AudSF VolChan Double
 read_volume =  arr (\v -> unsafePerformIO $ atomically $ peekTChan v)


wavSF :: FilePath -> IO (AudSF () Double)

                     inSig <- unsafePerformIO $ wavSFInf "input2.wav" -< ()
                     withVol <- read_volume v -< ()
                     appVol <- volume_control -< withVol
                     returnA -< appVol

> wavloop :: VolChan -> IO ()
> wavloop v =
>   let sigToPlay = ((unsafePerformIO $ wavSFInf "input2.wav") &&& read_volume v) >>> volume_control
>   in
>     do
>       playSignal 20 sigToPlay


 main :: IO ()
 main = do
  runUI "UI test" mixer_board
  wavloop

 kGUI :: Kleisli IO () Double
 kGUI = Kleisli (\x -> do runUI "test" mixer_board)

    d <- convertToUISF sr 0.1 myAutomaton -< (f1, f2)

 kAudio :: Kleisli IO Double ()
 kAudio = Kleisli (\v -> do wavloop v)

> main' :: IO ()
> main' = do
>  v <- newTChanIO
>  setNumCapabilities 2
>  forkOn 1 $ runUI "UI Demo" (mixer_board v)
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

an exampke of how to use klieslie

> cat :: Kleisli IO Int ()
> cat      = Kleisli (\x ->
>              do
>                putStrLn $ "cat" ++ show x
>                return ())
> dog :: Kleisli IO Int ()
> dog      = Kleisli (\x ->
>              do
>                putStrLn $ "dog" ++ show x
>                return ())
> catdog :: Kleisli IO Int ()
> catdog   = cat &&& dog >>> arr (\(x, y) -> x)

> h2Output :: IO()
> h2Output = runKleisli catdog 1
