> {-# LANGUAGE Arrows #-}
> module Main where
> import Euterpea
> import Euterpea.Experimental

> import Control.Arrow
> import Control.Concurrent
> import Control.Concurrent.STM
> import Control.Monad
> import System.IO.Unsafe


--------------------------------------------
here is the translated version of the below code


 main' :: IO ()
 main' = do
  x <- newTVarIO XDataDefaults
  setNumCapabilities 2
  forkOn 1 $  runMUI (defaultMUIParams {uiSize=(300,300), uiTitle="Instrument Demo"}) (mixer_board x)
  forkOn 2 $ wavloop x
  return ()

--------------------------------------------
here is the code I want the user to write

> f1 :: XData -> XData
> f1 i = XData (volume i) (pan' i) True

> f2 :: XData -> XData
> f2 i = XData (volume i) (pan' i) False

> main :: IO()
> main = do
>   let p1 = Process f1 1
>       p2 = Process f2 2
>   runP [p1,p2]

--------------------- -----------------------
here is the code to do the translation

> runP :: [Process] -> IO()
> runP ps = do
>    x <- newTVarIO $ xDataDefaults
>    setNumCapabilities (length ps)
>    mapM_ (\p -> forkOn (core p) $ foo x p) ps
>    return ()
>    where
>       xDataDefaults = XData 1 0.5 False
>       foo x p = writeT x ((func p) $ readT x)

> readT :: TVar XData -> XData
> readT x = unsafePerformIO $ atomically $ readTVar x
> writeT :: TVar XData -> XData -> IO()
> writeT x v = atomically $ writeTVar x v

> data XData = XData {volume :: Double,
>                pan' :: Double,
>                exit :: Bool}

> data Process = Process { func :: XData -> XData,
>                          core :: Int}

TVars are safe and slow

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f is equivalent to sequence_ . map f.
sequence_ :: Monad m => [m a] -> m ()
sequence_ =
    Evaluate each action in the sequence from left to right, and ignore the results.




> type VolChan = TVar Double --TChan Double
> type PanChan = TVar (Double,Double)
> type DevChan = TVar (Int,Int)

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
>    v2 <- volume_slider "track2" -< ()
>    _ <- uisfWriter vc -< v
>    _ <- uisfWriter pc -< v2
>    returnA -< ()

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
