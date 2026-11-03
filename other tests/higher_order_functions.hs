{-
  A higher order function in this case takes in a function as an argument.
  Say for example I make the `Add1` function `f(x) = x + 1`
-}
add1 :: Int -> Int
add1 x = x + 1

-- can also use anonymous add 1 function: 
-- (\ x -> x + 1)

-- application applys the add one function -- 
app :: (a -> b) -> a -> b
app f x = f x

test = app add1 
anon_test = app (\x -> x + 1)
anon_test2 = app (\x y z -> x + y + z) 7 8 9

{-
  Map is an important higher order function that takes in a function, 
  applies that function to every element of the list to output a different list.
-}
-- map :: (a -> b) -> [a] -> [b] --
map_test = map add1 [1,2,3] 
map_test2 = map (\(x, y) -> x + y) [(1,2), (3,4), (5,6)]

{-
  Filter is like map except the function (e.g. (a -> b) in map) in filter always
  must return a boolean. Sometimes also known as a predicate.
-}
-- filter :: (a -> Bool) -> [a] -> [a]
filter_test = filter (\x -> x > 2) [1,2,4,5,6,7]

-- maybe filter can be used to slice ? -- 
-- also note the dollar sign can eliminate the need for the outer parentheses around the filter call
slice :: [a] -> Int -> Int -> [a]
-- This slice will work but the zip term takes a long time to traverse and zip xs
-- slice xs from to = map (\(x, y) -> x) (filter (\(x, y) -> y >= from && y < to) (zip xs [0 .. length xs]))
-- slice xs from to = map (\(x, y) -> x) $ filter (\(x, y) -> y >= from && y < to) (zip xs [0 .. ])
-- There is a faster way:
slice xs from to
  | from < 0 || to <= from = []
  | otherwise = take (to - from) (drop from xs)


slice_test = slice [1 .. 10000000] 10000 10009