main :: IO()
main = do
    print $ digits 4321 == [4, 3, 2, 1]
    print $ digits 16543246 == [1, 6, 5, 4, 3, 2, 4, 6]

    print $ decreasing [4, 3, 2, 1] == True
    print $ decreasing [4, 3, 5, 1] == False
    print $ decreasing [4, 3, 3, 1] == False

    print $ decDigits 4321 == True
    print $ decDigits 4322 == False
    print $ decDigits 7635 == False

digits :: Int -> [Int]
digits n
 | n <= 9 = [n]
 |otherwise = (digits (n `div` 10)) ++ [(n `mod` 10)]

decreasing :: [Int] -> Bool
decreasing xs = all (\(x, y) -> x > y) (zip xs (tail xs))

decDigits :: Int -> Bool
decDigits = decreasing . digits