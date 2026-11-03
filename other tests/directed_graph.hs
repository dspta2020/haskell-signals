import Debug.Trace (traceShow)
{-
    Directed graph exercise:

    Basically create a function hasPath that determines if a valid path exists from one node to the other
    in this directed graph. Assumes that nodes 1 - 5 exist.
-}
edges = [(1,2), (2,3), (3,2), (4,3), (4,5)] :: [(Int, Int)]

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] x y = x == y
hasPath xs x y
  | x == y = True
  | otherwise =
      let xs'  = [(n, m) | (n, m) <- xs, n /= x]
          xs'' = [hasPath xs' m y | (n, m) <- xs, n == x]
      in traceShow ("xs'", xs') $
         traceShow ("xs''", xs'') $
         or xs''

test = hasPath edges 1 4