{-# LANGUAGE RecursiveDo, Arrows, TupleSections #-}

module UiExtras where

import FRP.UISF.SOE
import FRP.UISF.UITypes
import FRP.UISF.UISF
import FRP.UISF.AuxFunctions (SEvent, Time, timer, edge, delay, constA, concatA)

import Control.Arrow
import Data.Maybe (fromMaybe)

------------------------------------------------------------
-- Shorthand and Helper Functions
------------------------------------------------------------

-- Default padding between border and content
padding :: Int
padding = 3 

-- Introduce a shorthand for overGraphic
(//) :: Graphic -> Graphic -> Graphic
(//) = overGraphic


textboxH :: Int -> UISF String String
textboxH y = focusable $ 
  conjoin $ proc s -> do
    inFocus <- isInFocus -< ()
    k <- getEvents -< ()
    ctx <- getCTX -< ()
    rec let (s', i) = if inFocus then update s iPrev ctx k else (s, iPrev)
        iPrev <- delay 0 -< i
    displayStr -< seq i s'
    inf <- delay False -< inFocus
    b <- if inf then timer -< 0.5 else returnA -< Nothing
    b' <- edge -< not inFocus --For use in drawing the cursor
    rec willDraw <- delay True -< willDraw'
        let willDraw' = maybe willDraw (const $ not willDraw) b --if isJust b then not willDraw else willDraw
    canvas' displayLayout drawCursor -< case (inFocus, b, b', i == iPrev) of
              (True,  Just _, _, _) -> Just (willDraw, i)
              (True,  _, _, False)  -> Just (willDraw, i)
              (False, _, Just _, _) -> Just (False, i)
              _ -> Nothing
    returnA -< s'
  where
    minh = y*16 + padding * 2
    displayLayout = makeLayout (Stretchy 8) (Fixed minh)
    update s  i _ (Key c _ True)          = (take i s ++ [c] ++ drop i s, i+1)
    update s  i _ (SKey BACKSPACE _ True) = (take (i-1) s ++ drop i s, max (i-1) 0)
    update s  i _ (SKey DEL       _ True) = (take i s ++ drop (i+1) s, i)
    update s  i _ (SKey LEFT      _ True) = (s, max (i-1) 0)
    update s  i _ (SKey RIGHT     _ True) = (s, min (i+1) (length s))
    update s _i _ (SKey END       _ True) = (s, length s)
    update s _i _ (SKey HOME      _ True) = (s, 0)
    update s  i _ (SKey ENTER     _ True) = (take i s ++ "\\n" ++ drop i s, length s)
    update s _i c (Button (x,_) True True) = (s, min (length s) $ (x - xoffset c) `div` 8)
    update s  i _ _                        = (s, max 0 $ min i $ length s)
    drawCursor (False, _) _ = nullGraphic
    drawCursor (True, i) (w,_h) = 
        let linew = padding + i*8
        in if linew > w then nullGraphic else withColor Black $
            line (linew, padding) (linew, 16+padding)
    xoffset = fst . fst . bounds

textbox2 :: Int -> String -> UISF (SEvent String) String
textbox2 y startingVal = proc ms -> do
  rec s  <- delay startingVal -< ts
      ts <- textboxH y -< maybe s id ms
  returnA -< ts


------------------------------------------------------------
-- UI colors and drawing routine
------------------------------------------------------------

bg, gray0, gray1, gray2, gray3, blue3 :: RGB
bg = rgb 0xec 0xe9 0xd8
gray0 = rgb 0xff 0xff 0xff
gray1 = rgb 0xf1 0xef 0xe2
gray2 = rgb 0xac 0xa8 0x99
gray3 = rgb 0x71 0x6f 0x64
blue3 = rgb 0x31 0x3c 0x79

box :: [(RGB,RGB)] -> Rect -> Graphic
box [] _ = nullGraphic 
box ((t, b):cs) ((x, y), (w, h)) = 
  box cs ((x + 1, y + 1), (w - 2, h - 2)) 
  // withColor' t (line (x, y) (x, y + h - 1) 
                   // line (x, y) (x + w - 2, y)) 
  // withColor' b (line (x + 1, y + h - 1) (x + w - 1, y + h - 1) 
                   // line (x + w - 1, y) (x + w - 1, y + h - 1))

circle :: RGB -> Point -> Dimension -> Dimension -> Graphic
circle c (x, y) (w1, h1) (w2, h2) = 
  withColor' c $ arc (x + padding + w1, y + padding + h1) 
                     (x + padding + w2, y + padding + h2) 0 360

block :: Rect -> Graphic
block ((x,y), (w, h)) = polygon [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

pushed, popped, marked :: [(RGB,RGB)]
pushed = [(gray2, gray0),(gray3, gray1)]
popped = [(gray1, gray3),(gray0, gray2)]
marked = [(gray2, gray0),(gray0, gray2)]

inside :: Point -> Rect -> Bool
inside (u, v) ((x, y), (w, h)) = u >= x && v >= y && u < x + w && v < y + h


mkWidget :: s                                 -- ^ initial state
         -> Layout                            -- ^ layout
         -> (a -> s -> Rect -> UIEvent ->
             (b, s, DirtyBit))                -- ^ computation
         -> (Rect -> Bool -> s -> Graphic)    -- ^ drawing routine
         -> UISF a b
mkWidget i layout comp draw = proc a -> do
  rec s  <- delay i -< s'
      (b, s') <- mkUISF layout aux -< (a, s)
  returnA -< b
    where
      aux (ctx,f,t,e,(a,s)) = (db, f, g, nullTP, (b, s'))
        where
          rect = bounds ctx
          (b, s', db) = comp a s rect e
          g = scissorGraphic rect $ draw rect (snd f == HasFocus) s'


focusable :: UISF a b -> UISF a b
focusable (UISF layout f) = proc x -> do
  rec hasFocus <- delay False -< hasFocus'
      (y, hasFocus') <- UISF layout (h f) -< (x, hasFocus)
  returnA -< y
 where
  h fun (ctx, (myid,focus),t, inp, (a, hasFocus)) = do
    lshift <- isKeyPressed LSHIFT
    rshift <- isKeyPressed RSHIFT
    let isShift = lshift || rshift
        (f, hasFocus') = case (focus, hasFocus, inp) of
          (HasFocus, _, _) -> (HasFocus, True)
          (SetFocusTo n, _, _) | n == myid -> (NoFocus, True)
          (DenyFocus, _, _) -> (DenyFocus, False)
          (_, _,    Button pt _ True) -> (NoFocus, pt `inside` bounds ctx)
          (_, True, SKey TAB _ True) -> if isShift then (SetFocusTo (myid-1), False) 
                                                    else (SetFocusTo (myid+1), False)
          (_, _, _) -> (focus, hasFocus)
        focus' = if hasFocus' then HasFocus else DenyFocus
        inp' = if hasFocus' then (case inp of 
              SKey TAB _ _ -> NoUIEvent
              _ -> inp)
               else (case inp of 
              Button _ _ True -> NoUIEvent
              Key  _ _ _      -> NoUIEvent
              SKey _ _ _      -> NoUIEvent
              _ -> inp)
        redraw = hasFocus /= hasFocus'
    (db, _, g, cd, b, UISF newLayout fun') <- fun (ctx, (myid,focus'), t, inp', a)
    return (db || redraw, (myid+1,f), g, cd, (b, hasFocus'), UISF newLayout (h fun'))

isInFocus :: UISF () Bool
isInFocus = getFocusData >>> arr ((== HasFocus) . snd)

displayStrY :: Int -> UISF String ()
displayStrY y = mkWidget "" d (\v v' _ _ -> ((), v, v /= v')) draw 
  where
    minh = y*16 + padding * 2
    d = makeLayout (Stretchy 8) (Fixed minh)
    draw b@((x,y), (w, _h)) _ s = 
      let n = (w - padding * 2) `div` 8
      in withColor Black (text (x + padding, y + padding) (take n s)) 
         // box pushed b 
         // withColor White (block b)

displayStr :: UISF String ()
displayStr = mkWidget "" d (\v v' _ _ -> ((), v, v /= v')) draw 
  where
    minh = 16 + padding * 2
    d = makeLayout (Stretchy 8) (Fixed minh)
    draw b@((x,y), (w, _h)) _ s = 
      let n = (w - padding * 2) `div` 8
      in withColor Black (text (x + padding, y + padding) (take n s)) 
         // box pushed b 
         // withColor White (block b)

displayField :: UISF String ()
displayField = mkWidget "" layout (\v v' _ _ -> ((), v, v /= v')) draw
 where  
   layout = makeLayout (Stretchy (8 + padding * 2)) (Stretchy (16 + padding * 2))
   draw b@((x, y), (w, h)) _ s =
     let n = (w - padding * 2) `div` 8 -- number of chars that can fit on a line
         maxH = floor (fromIntegral (h - padding * 2) / fromIntegral 16) -- max height
         dfRecur c = if c == maxH - 1 
                       then dfRecurHelp c
                         else dfRecurHelp c // dfRecur (c + 1)
         dfRecurHelp c = withColor Black 
           (text (x + padding, y + padding + c * 16)
           (take n (drop (n * c) s)))
     in (dfRecur 0)
         // box pushed b
         // withColor White (block b)





canvas :: Dimension -> UISF (SEvent Graphic) ()
canvas (w, h) = mkWidget nullGraphic layout process draw 
  where
    layout = makeLayout (Fixed w) (Fixed h)
    draw ((x,y),(w,h)) _ = translateGraphic (x,y)
    process (Just g) _ _ _ = ((), g, True)
    process Nothing  g _ _ = ((), g, False)

canvas' :: Layout -> (a -> Dimension -> Graphic) -> UISF (SEvent a) ()
canvas' layout draw = mkWidget Nothing layout process drawit
  where
    drawit (pt, dim) _ = maybe nullGraphic (\a -> translateGraphic pt $ draw a dim)
    process (Just a) _ _ _ = ((), Just a, True)
    process Nothing  a _ _ = ((), a, False)
