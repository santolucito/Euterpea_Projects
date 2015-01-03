
1. Here is one: define a function that takes two lists, both of which have elements in growing order (i.e 1,2,3,3,5,9,9,..), and outputs a list with the type of [Either a a], where a `Left a' would represent an element from the first list, a `Right a' from the second list. Make it so that the elements are in order, and also that the lefts preceede the rights.

Example, the inputs of [1,1,3,7,7] and [1,2,3,5,5] would result in [Left 1, Left 1, Right 1, Right 2, Left 3, Right 3, Right 5, Right 5, Left 7, Left 7]

> list1 :: [a] -> [a] -> [Either a a]
> list1 (x:xs) (y:ys) = [Left x] ++ [Right y] ++ list1 xs ys
> list1 [] (y:ys) = [] ++ [Right y] ++ list1 [] ys
> list1 (x:xs) [] = [Left x] ++ [] ++ list1 xs []
> list1 [] [] = [] ++ []

.: takes a one arg func on left and two arg func on right

> (.:) = (.).(.)

> list1' :: [a] -> [a] -> [Either a a]
> list1' = (concat . map foo) .: zip  where
>   foo (x,y) = [Left x] ++ [Right y]

2. As an extension (not sure if this is too difficult for what you're looking for): write another function that takes a list-of-lists as input
and outputs the result in tuples, where the first member is the index of the list (counting from zero), and the second is the value itself.

Example [[1,3],[2,3],[1,3,4]] should result in [(0,1),(2,1),(1,2),(0,3),(1,3),(2,3),(2,4)]

> list2 :: [[a]] -> [(Int,a)]
> list2 (x:xs) = foo x 0 ++ list2 xs where
>   foo :: [a] -> Int -> [(Int,a)]
>   foo (x:xs) ctr = [(ctr,x)] ++ foo xs (ctr+1)
>   foo [] _ = []
> list2 _ = []


> list2' :: [[a]] -> [(Int,a)]
> list2' = concat . map (foo 0) where
>   foo :: Int -> [a] -> [(Int,a)]
>   foo ctr (x:xs) = [(ctr,x)] ++ foo (ctr+1) xs
>   foo _ [] = []
