> module HW4 where
>
> import Euterpea

Exercise 8.14

> data MyMusic a  = 
>       MyPrim (Primitive a)               -- primitive value 
>    |  MyMusic a :+ MyMusic a              -- sequential composition
>    |  MyMusic a := MyMusic a              -- parallel composition
>    |  MyMusic a :=/ MyMusic a              -- truncating parallel composition
>  deriving (Show, Eq, Ord)
>
> myDur                       :: MyMusic a -> Dur
> myDur (MyPrim (Note d _))     = d
> myDur (MyPrim (Rest d))       = d
> myDur (m1 :+ m2)           = myDur m1   +   myDur m2
> myDur (m1 := m2)           = myDur m1 `max` myDur m2
> myDur (m1 :=/ m2)          = myDur m1
>
> myTakeM :: Dur -> MyMusic a -> MyMusic a
> myTakeM d m | d <= 0            = (MyPrim(Rest 0))
> myTakeM d (MyPrim (Note oldD p))  = (MyPrim (Note (min oldD d) p))
> myTakeM d (MyPrim (Rest oldD))    = (MyPrim (Rest (min oldD d)))
> myTakeM d (m1 := m2)           = myTakeM d m1 := myTakeM d m2
> myTakeM d (m1 :+ m2)           = let  m'1  = myTakeM d m1
>                                       m'2  = myTakeM (d - myDur m'1) m2
>                                  in m'1 :+ m'2

The second argument must an infinite-duration music value for :=/

> myPerform :: PTime -> MyMusic Pitch -> Performance
> myPerform t (MyPrim (Note d p)) = [Event t AcousticGrandPiano
>                                         (absPitch p) d 127 []]
> myPerform t (MyPrim (Rest d))   = []
> myPerform t (m1 :+  m2)         = myPerform t m1 ++ myPerform (t+(myDur m1)) m2
> myPerform t (m1 :=  m2)         = merge (myPerform t m1) (myPerform t m2)
> myPerform t (m1 :=/ m2)         = merge (myPerform t m1) (myPerform t (myTakeM (myDur m1) m2))

Exercise 18.1

violin string -       transverse
stop-and-go traffic - longitudinal
the "wave" -          transverse
water hammer  -       longintudinal
stone in lake -       transverse
radio wave -          transverse

Exercise 18.2

> soundms = 340.29
> distance182 = soundms * 5

Exercise 18.3

sounds must travel both ways, so need 1 sec

> distance183 = soundms * 1

Exercise 18.4

We need a factor of 2 increase
Sdb = 10 log10 (S/R)
simply increase log10(S/r) by 1, ie double S

Exercise 18.5

> dog = 20 * log (45000/60)
> bat = 20 * log (110000/2000)
> humam = 20 * log (20000/20)

dynamic range of human ear = 138 dB
dynamic range of dog ear = 132 dB
dynamic range of bat ear = 80

Exercise 18.6

interesting, this version of list compreshension is as lazy as i expected
you cant do length on it, but you can type it in and count
oh well, gets the job done... wonder why tho???

> overtones1  = [100*2^x | x <- [0..], 100*2^x <= 20000]
> overtones2  = [500*2^x | x <- [0..], 500*2^x <= 20000]
> overtones3  = [1500*2^x | x <- [0..], 1500*2^x <= 20000]
> overtones4  = [5000*2^x | x <- [0..], 5000*2^x <= 20000]

8 overtones for 100 Hz
6 overtones for 500 Hz
4 overtones for 1500 Hz
3 overtones for 5000 Hz

Exercise 18.7

> r :: Int -> Int -> Int
> r f s = (f*s) `quot` (lcm (f) (s)) 

Exercise 18.8

it take 4 megabytes
3 min * 60 sec/min * 44,100 samples/sec * 16 bit/sample / 8388608 bits/MB

> memory  = 3 * 16 * 44100 * 16 / 8388608

Exercise 18.9

sampling at 44.1, we want 44100 sample table size, so that i will always be an integer 

Exercise 18.10

the sound wave will need to be compressed to half its original size so the police car needs to go half the speed of sound

> doppler v = 1 / ( 1 - v / soundms)
> tester = 2.0 == doppler (soundms/2)

