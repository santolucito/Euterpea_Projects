> {-# LANGUAGE Arrows #-}
> module Main where
> import Euterpea
> import Euterpea.Experimental

> import Control.Arrow
> import Control.Concurrent
> import Control.Concurrent.STM
> import Control.Monad
> import System.IO.Unsafe

> import ExperimentalMidi

STM

> type VolChan = TVar Double --TChan Double
> type PanChan = TVar (Double,Double)
> type DevChan = TVar (Int,Int)

GUI

the problem with convertToUISF is that then you ahve to write enough samples
to fill the buffer until the next tick of the clock (uisf at 60fps, audsf at 44k)
so this is a bad idea
    _ <- convertToUISF 60 60

apparently every uisf widget thing needs a unique title

> volume_slider :: String -> UISF () (Double)
> volume_slider t = proc _ -> do
>    a <- title t $ vSlider (0,1) 0 -< ()
>    _ <- display -< 1-a
>    outA -< 1-a

> pan_slider :: UISF () (Double,Double)
> pan_slider = proc _ -> do
>    a <- title "pan"  $ hSlider (0,1) 0 -< ()
>    _ <- display -< (a,1-a)
>    outA -< (a,1-a)

> uisfWriter :: TVar a -> UISF (a) ()
> uisfWriter v = liftAIO (\x -> atomically $ writeTVar v x)

> mixer_board :: VolChan -> VolChan -> UISF () ()
> mixer_board vc pc = title "Mixer" $ leftRight $ proc _ -> do
>    v <- volume_slider "track1" -< ()
>    _ <- uisfWriter vc -< v
>    v2 <- volume_slider "track2" -< ()
>    _ <- uisfWriter pc -< v2
>    returnA -< ()

mi::Int

> --haskellOx :: (DevChan -> UISF () ()
> haskellOx devsIn devsOut devChan = proc _ -> do
>   rIn <- title "Input" $ radio (map snd devsIn) 0 -< ()
>   rOut <- title "Output" $ radio (map snd devsOut) 0 -< ()
>   let inVal = map fst devsIn !! rIn
>       outVal = map fst devsOut !! rOut
>   _  <- uisfWriter devChan -< (inVal, outVal)
>   returnA -< ()

Audio

> toMono :: AudSF (Double, Double) (Double)
> toMono =  arr (\(l,r) -> (l+r)/2)

> -- volume_control :: AudioSample a => AudSF (a, Double) (a)
> volume_control :: AudSF ((Double,Double), Double) (Double,Double)
> volume_control = arr (\((l,r),v) -> (l*v,r*v))

> pan :: AudSF ((Double,Double), (Double,Double)) (Double,Double)
> pan = arr (\((l,r),(v1,v2)) -> (l*v1,r*v2))

> mix2 :: AudSF ((Double,Double), (Double,Double)) (Double,Double)
> mix2 = arr (\((l,r),(v1,v2)) -> ((l+v1)/2,(r+v2)/2))

> -- sfReader :: (Arrow a, Chan b) => Chan b -> (a () b)
> sfReader :: TVar a -> (AudSF () a)
> sfReader v =  arr (\x -> unsafePerformIO $ atomically $ readTVar v)


> wavloop :: VolChan -> VolChan -> IO ()
> wavloop v v2 = do
>   foo <- wavSFStereoInf "in1.wav"
>   bar <- wavSFStereoInf "in2.wav"
>   let sigPlay = ((((foo) &&& sfReader v) >>> volume_control) &&&
>                  (((bar) &&& sfReader v2) >>> volume_control)) >>> mix2 >>> toMono
>   playSignal 1000 sigPlay

> midi :: IO ()
> midi = do
>  devsIn <- inDevices
>  devsOut <- outDevices
>  v <- newTVarIO 0.2
>  s <- newTVarIO False
>  d <- newTVarIO (0,0)
>  setNumCapabilities 2
>  forkOn 1 $ runMUI (defaultMUIParams {uiSize=(300,300), uiTitle="HaskellOx"}) (haskellOx devsIn devsOut d)
>  forkOn 2 $ midiInLoop s d
>  return ()

> wav :: IO ()
> wav = do
>  v <- newTVarIO 0.2
>  v2 <- newTVarIO 0.2
>  p <- newTVarIO (1,1)
>  setNumCapabilities 2
>  forkOn 1 $  runMUI (defaultMUIParams {uiSize=(300,300), uiTitle="Instrument Demo"}) (mixer_board v v2)
>  forkOn 2 $ wavloop v v2
>  return ()



 forkOn 2 $ outFile "test.wav" 500 $ sfReader v
 forkOn 2 $ forever $ (atomically $ isEmptyTChan v) >>= print
 forkOn 2 $ forever $ (atomically $ readTVar d) >>= print







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
