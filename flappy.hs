
{-| A Flappy bird/ helicopter game clone -}
module Flappy where

import FRP.Helm
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Text as Text
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keys
import qualified FRP.Helm.Sample as S

import Data.List
-- MODEL
data GameObject =  Character { y :: Double,
                               energy :: Double,
                               active :: Bool}
                 | Obstacle { x :: Double,
                              y :: Double,
                              which :: Int}
                 | TextBox { i :: Int}
                 | VolChan { c :: TVar Double,
                             i :: ()}--should be an IO to execute somewhere else

bird = Character 300 750 True

obs1 = Obstacle 800 350 1
obs2 = Obstacle 1200 200 3
obs3 = Obstacle 1450 200 5

score = TextBox 0

allObs = [obs1,obs2,obs3]

obsPositions = cycle $ concat $ permutations [130,433,300,210,540,500,200,450,333,100,400]

-- UPDATE

step :: (Time,[Bool]) -> [GameObject] -> [GameObject]
step keys cs = map (update keys) cs

update :: (Time,[Bool]) -> GameObject -> GameObject
update (t,keys) (Character y e a)
    | a == False = Character y e a
    | y > 550 = Character (550) e True
    | y < 0 = Character (0) e True
    | e > 751 = Character (y+1.5) (750) True
    | e < 0 = Character (y+1.5) (0) True
    | keys!!0 == True = Character (y-3) (e-3) True
    | keys!!1 == True = Character (y+6) (e+1.5) True
    | keys!!0 == False = Character (y+1.5) (e+1.5) True

update (t,keys) (Obstacle x y w)
    | x < 0 = Obstacle (800) (obsPositions!!w) (w+1)
    | x >= 0 = Obstacle (x-(fromIntegral w/5)-5) y w

update (t,keys) (TextBox i) = TextBox $ i+1

update (t,keys) (VolChan v) =
    | keys!!0 == True = VolChan v (unsafePerformIO $ atomically $ writeTVar v 0)
    | keys!!1 == True = VolChan v (unsafePerformIO $ atomically $ writeTVar v 1)
    | keys!!0 == False =  VolChan v (unsafePerformIO $ atomically $ writeTVar v 1)

-- DISPLAY

--need to scale to windows dimensions
render :: [GameObject] -> (Int,Int) -> Element
render cs (w,h) = collage w h $ concat (map my_collage cs)

my_collage :: GameObject -> [Form]
my_collage (Character y e a) = (healthBar e) ++ (player y)
my_collage (Obstacle x y w) = obs x y w
my_collage (TextBox i) = [move (300,9) $ toForm $ Text.text $ Text.color white $ Text.toText $ "Score: "++show i]

healthBar :: Double -> [Form]
healthBar e = [rect ((*2) $ e) 35 |> filled green]
player y = [move (400, y) $ toForm $ image 50 50 "player.png"]
obs x y w = [move (x,y) $ filled red $ rect 25 100]



-- INPUT
runAt c s = let x = lift2 (,) c s
            in lift snd x

getKeys= [(Keys.isDown Keys.SpaceKey), (Keys.isDown Keys.FKey)]

input :: Signal (Time,[Bool])
input = Time.timestamp $ runAt (Time.fps 60) $ combine getKeys


{-| Bootstrap the game. -}
flap :: TVar Double -> IO ()
flap v = do
    run config $ render <~ stepper ~~ Window.dimensions

  where
    config = defaultConfig { windowTitle = "Helm - Flappy" }
    stepper = foldp step ([bird]++allObs++[score]++(VolChan v)) input
