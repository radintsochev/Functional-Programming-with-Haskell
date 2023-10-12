main :: IO()
main = do
    print $ prodOdds [1, 2, 3, 4, 5, 6] == 48
    print $ prodOdds [7.66, 7, 7.99, 7] == 49.0

prodOdds :: (Num a) => [a] -> a
prodOdds = product . (map (\(x1, x2) -> x2)) . (filter (\(x1, x2) -> x1 `mod` 2 == 1)) . (zip [0 ..])