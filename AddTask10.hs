main :: IO()
main = do
    print $ (getIndices [2, 7, 11, 15]) 9 == (0,1)
    print $ (getIndices [3, 2, 4]) 6 == (1,2)
    print $ (getIndices [3, 3]) 6 == (0,1)

permutations :: [a] -> [(a, a)]
permutations [x] = []
permutations xs = (zip (repeat (head xs)) (tail xs) ) ++ permutations (tail xs)

getIndices :: [Int] -> (Int -> (Int, Int))
getIndices xs = (\x -> head [(yi, zi) | ((y, yi), (z, zi)) <- permutations (zip xs [0 ..]), y + z == x, yi < zi])