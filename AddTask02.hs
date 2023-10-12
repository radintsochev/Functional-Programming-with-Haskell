main :: IO()
main = do
    print $ (sumExpr (2+) [0, 1, 2, 3]) 2 == 80
    print $ (sumExpr (*0.8) [0, 1, 2, 3, 4, 5]) 10 == 4345680.0

sumExpr :: (Num a) => (a -> a) -> [a] -> (a -> a)
sumExpr f ys = (\a -> sum (zipWith (*) ys (map f (zipWith (^) (replicate (length ys) a) [1 ..]))))