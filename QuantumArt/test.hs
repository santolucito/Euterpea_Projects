loop sf =
 \s -> 
   let 
     f 0 = (s(0),undefined)
     f t = (s(t), snd ( sf f (t-1)))
   in
     \t -> fst (sf f t)
   


