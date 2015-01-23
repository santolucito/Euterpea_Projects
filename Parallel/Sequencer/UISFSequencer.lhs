> {-# LANGUAGE Arrows #-}

> module UISFSequencer where
> import Euterpea
> import Euterpea.ExperimentalPlay
> import Control.Monad

> import Control.Concurrent.STM
> import System.IO.Unsafe
> import Control.Concurrent
> import FRP.UISF.AuxFunctions
> import Control.Applicative

---------
visual (UISF)

> game v = runMUI (defaultMUIParams {uiSize=(300,320), uiTitle="Instrument Demo"})
>                (mixer_board v)

> boxes :: String -> UISF () ([Int])
> boxes t = title t $ leftRight $ proc _ -> do
>   x <- concatA $ replicate 8 (checkbox "" False) -< cycle [()]
>   outA -< map bin x
>   where
>     bin True = 1
>     bin False = 0

> uisfWriter :: TVar a -> UISF (a) ()
> uisfWriter v = liftAIO (\x -> atomically $ writeTVar v x)

> mixer_board :: TVar ([[Int]]) -> UISF () ()
> mixer_board vc = title "UISF Sequencer" $ topDown $ proc _ -> do
>   mo    <- selectOutput -< ()
>   display -< mo
>   x <- concatA $ replicate 5 (boxes "") -< cycle [()]
>   _ <- uisfWriter vc -< x
>   returnA -< ()
