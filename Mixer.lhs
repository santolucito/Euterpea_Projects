> {-# LANGUAGE Arrows #-}

try and play two samples
one after another

> module Main where
> import Euterpea hiding (MUI)
> --import Euterpea.IO.Audio.PortAudioChannel
> import Control.Arrow
> import Control.Concurrent
> import Control.Concurrent.STM

> import Control.Monad

> import Euterpea.Experimental
> import System.IO.Unsafe

> import ExperimentalMidi(midiInLoop,midiOutOp)

STM

> type VolChan = TVar Double --TChan Double

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

> uisfWriter :: VolChan -> UISF (Double) ()
> uisfWriter v = liftAIO (\x -> atomically $ writeTVar v x)

> mixer_board :: VolChan -> UISF () ()
> mixer_board vc = title "Mixer" $ proc _ -> do
>    v <- volume_slider -< ()
>    _ <- uisfWriter vc -< v --1-v
>    returnA -< ()

Audio

> volume_control :: AudSF (Double, Double) (Double)
> volume_control = arr (\(s,v) -> (s* v))

> -- sfReader :: (Arrow a, Chan b) => Chan b -> (a () b)
> sfReader :: VolChan -> (AudSF () Double)
> sfReader v =  arr (\x -> unsafePerformIO $ atomically $ readTVar v)
>  where
>    g Nothing = 0.5
>    g (Just x) = x

> wavloop :: VolChan -> IO ()
> wavloop v =
>   let
>    sigToPlay = ((unsafePerformIO $ wavSFInf "input2.wav") &&& sfReader v) >>> volume_control
>   in
>     do
>       playSignal 20 sigToPlay

 foo <- wavSFInf "input2.wav"
  let sigPlay = (foo &&& sfReader v) >>> volume_control
   playSignal 20 sigPlay

playSignal wants a pu re signal,
need a playImpureSignal


> main' :: IO ()
> main' = do
>  v <- newTVarIO 0.2
>  setNumCapabilities 2
>  forkOn 1 $ runMUI' "UI Demo" (mixer_board v)
>  forkOn 2 $ midiOutOp v
>  --forkOn 2 $ wavloop v
>  return ()


 forkOn 2 $ outFile "test.wav" 500 $ sfReader v
 forkOn 2 $ forever $ (atomically $ isEmptyTChan v) >>= print







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

wavSF :: FilePath -> IO (AudSF () Double)

                     inSig <- unsafePerformIO $ wavSFInf "input2.wav" -< ()
                     withVol <- read_volume v -< ()
                     appVol <- volume_control -< withVol
                     returnA -< appVol

 main :: IO ()
 main = do
  runUI "UI test" mixer_board
  wavloop

 kGUI :: Kleisli IO () Double
 kGUI = Kleisli (\x -> do runUI "test" mixer_board)

    d <- convertToUISF sr 0.1 myAutomaton -< (f1, f2)

 kAudio :: Kleisli IO Double ()
 kAudio = Kleisli (\v -> do wavloop v)

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
