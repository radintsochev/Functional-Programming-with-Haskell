digits :: Int -> [Int]
digits n
 | n <= 9 = [n]
 | otherwise = (digits (n `div` 10)) ++ [n `mod` 10]

unDigits :: [Int] -> Int -> Int
unDigits [] _ = 0
unDigits (xs) helper = (last xs) * (10 ^ helper) + unDigits (init xs) (helper + 1)

squareDigits :: Int -> Int
squareDigits n = unDigits (map (^2) (digits n)) 0
