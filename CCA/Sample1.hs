{-# LANGUAGE Arrows #-}
module Sample1 where


import Control.Arrow
import Control.CCA.Types
import Prelude hiding (init, exp)

import qualified Data.List.Stream as S


--this was taken from Paul Liu from his CCA paper
--http://code.haskell.org/CCA/test/
--I am tring to rewrite these without arrows (normal FRP)

--two different timings exits
-- http://www.thev.net/PaulLiu/download/jfp092011.pdf
-- http://www.thev.net/PaulLiu/download/icfp066-liu.pdf


--exp is a recursive definition for 2^n by using integrals
--which is a retarted way to do this

--exp with Arrow

sr = 44100 :: Int
dt = 1 / (fromIntegral sr)
--dt =1

exp :: ArrowInit a => a () Double
exp = proc () -> do
  rec let e = 1 + i
      i <- integral -< e
  returnA -< e

integral :: ArrowInit a => a Double Double
integral = proc e -> do
  rec let i' = i + e * dt
      i <- init 0 -< i' --init is id when t !=0
  returnA -< i

--exp with nonArrow
--a is a value, b is a state
--running at same as fast tuple arrow
nth_FRP :: Int -> (b, (b -> (a,b))) -> a
nth_FRP n (i,f) = aux n i
  where
    aux n i = x `seq` if n == 0 then x else aux (n-1) i'
      where (x, i') = f i

--my old version
--running at ~90x
{-nth_FRP :: Double -> (Double -> (a,Double)) -> a
nth_FRP n f = nth_FRP' n 0 0 f

nth_FRP' :: Double -> Double -> Double -> (Double -> (a,Double)) -> a
nth_FRP' n t i f =
  let x = f $! i
  in
    if t == n then (fst x) else nth_FRP' n (t+1) (snd x) f
-}


exp' :: Double -> (Double,Double)
exp' i =
  let e = 1+i
      i'= i+e*dt
  in
      (e,i')


{-
--4.5x
--last $ take n S.exp'
exp' :: [Double]
exp' =
  let i = 0: i'
      e = S.map (+1) i
      i' = S.zipWith (\x y -> x+y*dt) i e
  in
      e
-}



{-

--gives you a different value than arrows with dt !=1
--also is really slow, probably using an integral is a
--faster way to do ** than calculating ** from scratch everytime
--with the integral, you already ahve most of the computation done
nth_FRP n t f = (f t) `seq` if t == n then f t else nth_FRP n (t+1) f
exp' :: Double -> Double
exp' t  = 2**(t* dt)

exp'' :: Double -> (Double,Double)
exp'' t = (t+1,2**(t*dt))

exp' :: (Double,Double)
exp' =
   let
     x = (2**(t*dt),t+1)
     t = snd x
   in
     x
-}





sine :: ArrowInit a => Double -> a () Double
sine freq = proc _ -> do
  rec x <- init i -< r
      y <- init 0 -< x
      let r = c * x - y
  returnA -< r
  where
    omh = 2 * pi / (fromIntegral sr) * freq
    i = sin omh
    c = 2 * cos omh

sineL :: Double -> [Double ]
sineL freq =
  let omh = 2 * pi * freq * dt
      d = sin omh
      c = 2 * cos omh
      r = zipWith (\d2 d1 -> c * d2 - d1 ) d2 d1
      d1 = delay 0 d2
      d2 = delay d r
  in r
delay = (:)



oscSine :: ArrowInit a => Double -> a Double Double
oscSine f0 = proc cv -> do
  let f = f0 * (2 ** cv)
  phi <- integral -< 2 * pi * f
  returnA -< sin phi

testOsc :: ArrowInit a => (Double -> a Double Double) -> a () Double
testOsc f = arr (const 1) >>> f 440

sciFi :: ArrowInit a => a () Double
sciFi = proc () -> do
   und <- oscSine 3.0 -< 0
   swp <- integral -< -0.25
   audio <- oscSine 440 -< und * 0.2 + swp + 1
   returnA -< audio

robot :: ArrowInit a => a (Double, Double) Double
robot = proc inp -> do
    let vr = snd inp
        vl = fst inp
        vz = vr + vl
    t <- integral -< vr - vl
    let t' = t / 10
    x <- integral -< vz * cos t'
    y <- integral -< vz * sin t'
    returnA -< x / 2 + y / 2

testRobot :: ArrowInit a => a (Double, Double) Double -> a () Double
testRobot bot = proc () -> do
    u <- sine 2 -< ()
    robot -< (u, 1 - u)
