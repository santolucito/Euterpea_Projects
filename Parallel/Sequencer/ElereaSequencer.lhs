> {-# LANGUAGE RecursiveDo #-}
>
> module ElereaSequencer where
>
> import Control.Applicative
> import Control.Concurrent
> import Control.Monad
> import Data.IORef
> import Data.List
> import Data.Maybe
> import Data.Traversable hiding (mapM)
> import FRP.Elerea.Param
> import Graphics.UI.GLFW as GLFW
> import Graphics.Rendering.OpenGL
> import System.Environment
> import System.IO.Unsafe

> import Common.Utils
> import Common.Vector

> import Control.Concurrent.STM
> import System.IO.Unsafe

Global constants
----------------

The dimensions and vertical position of the player.

> playerW = 0.2
> playerH = 0.03
> playerY = -fieldH+0.01

The dimensions of the field.

> fieldW = 0.8
> fieldH = 0.6

The dimensions of each brick.

> brickW = 0.2
> brickH = 0.2

The data structure describing the state of each brick.  A brick can
either be alive or dying.  Dying bricks also keep track of their
fadeout level.

> data BrickState = Live | Dead

The positions of the bricks.

> brickPos = distributeBricks (-0.7) (-0.3) (0.7) (0.4) 5 3
>     where distributeBricks xmin ymin xmax ymax xn yn = [(xmin+xstep*x,ymin+ystep*y) |
>                                                         x <- [0..xn-1], y <- [0..yn-1]]
>               where xstep = (xmax-xmin-xn*brickW)/(xn-1)+brickW
>                     ystep = (ymax-ymin-yn*brickH)/(yn-1)+brickH



Game logic
----------

The entry point performs some lightweight initialisation, and defines
the two user-driven signals: window size and mouse position.  The
`external` function creates the signal and the corresponding sink at
the same time.

When all is done `driveNetwork` is invoked.  It is not a library
function, but part of the tiny `Utils` module .

main :: IO()
 main = do

> game :: TVar [[Int]] -> IO()
> game v = do
>   -- Creating a window without a depth buffer
>   initialize
>   openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8] Window
>   windowTitle $= "Elerea Breakout"
>
>   -- External signals available for the game logic
>   (windowSize,windowSizeSink) <- external vnull
>   (mousePosition,mousePositionSink) <- external vnull
>
>   -- Wrapping up the init phase
>   closed <- newIORef False
>   windowSizeCallback $= resizeGLScene windowSizeSink
>   windowCloseCallback $= (writeIORef closed True >> return True)
>   initGL 640 480
>
>   -- All we need to get going is an IO-valued signal and an IO
>   -- function to update the external signals
>   game <- start (breakout mousePosition windowSize v)
>   driveNetwork game (readInput mousePositionSink closed)
>
>   closeWindow


> breakout mousePos windowSize v = do

User-driven player position:

>   rec let playerX = adjustPlayerPos <$> mousePos <*> windowSize
>           adjustPlayerPos (V x _) (V w _) = min (fieldW-playerW) $ max (-fieldW) $ 2*x/w-1-playerW/2
>           toMaybe c v = if c then Just v else Nothing

>   return $ renderLevel v <$> playerX

> renderLevel v playerX = do
>   let drawRect x y xs ys = do
>         loadIdentity
>         renderPrimitive Quads $ do
>           vertex $ Vertex3 (x)    (y)    (0 :: GLfloat)
>           vertex $ Vertex3 (x+xs) (y)    (0 :: GLfloat)
>           vertex $ Vertex3 (x+xs) (y+ys) (0 :: GLfloat)
>           vertex $ Vertex3 (x)    (y+ys) (0 :: GLfloat)
>
>   clear [ColorBuffer]
>
>   color $ Color4 0.2 0.2 0.2 (1 :: GLfloat)
>   drawRect (-fieldW) (-fieldH) (fieldW*2) (fieldH*2)
>

  forM_ bricks $ \(x,y,s) -> do
    case s of
      Live    -> color $ Color4 0.8 0.5 0.5 (0.6 :: GLfloat)
      Dead -> color $ Color4 0.9 0.9 0.2 (0.0 :: GLfloat)
    drawRect x y brickW brickH

>
>   color $ Color4 0.3 0.4 0.8 (0.5 :: GLfloat)
>   drawRect playerX playerY playerW playerH
>
>   flush
>   swapBuffers

Backend
-------

The `readInput` function has two responsibilities: it provides input
for the `mousePosition` peripheral signal through its associated sink,
and also feeds the time difference between two states into the system,
deciding when to exit altogether (by returning `Nothing` instead of
the current `dt` value wrapped in `Just`).

The `threadDelay` call at the beginning is just a trick to give the
scheduler a breath.  It will cause a wait equal to a scheduler tick,
which is 20ms by default.  The program can run perfectly without it,
but it eats up all the free CPU to produce an unnecessarily high frame
rate.

> readInput mousePos closed = do
>   threadDelay 0
>   t <- get GLFW.time
>   GLFW.time $= 0
>   Position x y <- get GLFW.mousePos
>   mousePos (V (fromIntegral x) (fromIntegral y))
>   k <- getKey ESC
>   c <- readIORef closed
>   return (if c || k == Press then Nothing else Just t)

The `initGL` function sets up almost nothing, which means that most
functionality is turned off.  Only alpha blending is enabled to
provide some minimalistic eye candy.

> initGL width height = do
>   clearColor $= Color4 0 0 0 1
>   blend $= Enabled
>   blendFunc $= (SrcAlpha,OneMinusSrcAlpha)
>   cullFace $= Just Back

The resize callback feeds the `windowSize` signal through its sink
besides adjusting the projection matrix.

> resizeGLScene winSize size@(Size w h) = do
>   winSize (V (fromIntegral w) (fromIntegral h))
>
>   viewport $= (Position 0 0,size)
>
>   matrixMode $= Projection
>   loadIdentity
>   scale 1 (fromIntegral w/fromIntegral h) (1 :: GLfloat)
>
>   matrixMode $= Modelview 0
