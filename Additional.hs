import Data.Char
import Data.List

isInteresting :: Int -> Bool
isInteresting n = n `mod` (sum $ map digitToInt $ show n) == 0

specialSum :: Int -> Int -> Int
specialSum x y = sum [m | m <- [x..y], elem '6' (show m), m `mod` 4 == 1]

mySin :: Integer -> Double -> Double
mySin 0 x = x
mySin n x = ((-1)^n * x^(2*n + 1)) / (fromIntegral $ product [1 .. 2*n + 1]) + mySin (n - 1) x

dominates :: (Ord a, Num a) => (a -> a) -> (a -> a) -> [a] -> Bool
dominates f g = all (\ x -> abs (f x) >= abs (g x)) 

reverseOrdSuff :: Int -> [Int]
reverseOrdSuff n =  zipWith (\x y -> if x < y then x else 0) (reverseDigits n) ((tail $ reverseDigits n) ++ [0])
 where
    reverseDigits:: Int -> [Int]
    reverseDigits n = reverse $ map digitToInt $ show n

sumUnique :: [[Int]] -> Int
sumUnique = sum . map sumUnq 
 where
    sumUnq :: [Int] -> Int
    sumUnq = sum . concat . filter ((==1) . length) . group . sort