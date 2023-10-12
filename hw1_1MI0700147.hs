main::IO()
main = do
    print $ eqSumPowDig 100 2 == 0
    print $ eqSumPowDig 1000 2 == 0
    print $ eqSumPowDig 2000 2 == 0
    print $ eqSumPowDig 200 3 == 153
    print $ eqSumPowDig 370 3 == 523
    print $ eqSumPowDig 370 3 == 523
    print $ eqSumPowDig 400 3 == 894
    print $ eqSumPowDig 500 3 == 1301
    print $ eqSumPowDig 1000 3 == 1301
    print $ eqSumPowDig 1500 3 == 1301
    print $ getNthSevenlikeNum 1 == 1
    print $ getNthSevenlikeNum 2 == 7
    print $ getNthSevenlikeNum 3 == 8
    print $ getNthSevenlikeNum 4 == 49

--A function that calculates the sum of the digits of a given natural number risen to a power
sumPowDig :: Int -> Int -> Int
sumPowDig 0 _ = 0
sumPowDig n k = (n `mod` 10)^k + sumPowDig (n `div` 10) k

--A function that checks if a given number is special
isSpecial :: Int -> Int -> Bool
isSpecial n k = n == sumPowDig n k

{-
A function that goes through all the numbers less or equal to the 
upper limit and if they are "special" adds them to the final sum
-}
eqSumPowDig :: Int -> Int -> Int
eqSumPowDig hMax k
    | hMax < 0 = error "The upper limit must be a natural number!"
    | k < 0 = error "The power must be a natural number!"
    | hMax == 1 = 0
    | otherwise = helper 2 0
 where
    helper:: Int -> Int -> Int
    helper curr sum = if curr == hMax + 1
        then sum
        else if isSpecial curr k
            then helper (curr + 1) (sum + curr)
            else helper (curr + 1) (sum)

--A function that checks if a given number is a power of 2
isPowerOfTwo :: Int -> Bool
isPowerOfTwo 1 = True
isPowerOfTwo n = if n `mod` 2 == 0
    then isPowerOfTwo (n `div` 2)
    else False

--A function that calculates the logarithm with base 2 of a given power of 2
logBaseTwo :: Int -> Int
logBaseTwo 1 = 0
logBaseTwo n = 1 + logBaseTwo (n `div` 2)

--A functions that finds the nearest power of 2 to a number that isn't greater than the number
floorToPowerOfTwo :: Int -> Int
floorToPowerOfTwo n = if isPowerOfTwo n 
    then n
    else floorToPowerOfTwo (n - 1)               

{-
List of sevenlike numbers:
(1)  (2)     (3)     (4)
 ^    ^       ^       ^
 |    |       |       |
7^0, 7^1, 7^1 + 7^0, 7^2, (4) + (1), (4) + (2), (4) + (3), (5), ...
... (n), (n) + (1), ... , (n) + (n-1), (n+1), ...
where 1, 2, 4, ..., n, n+1 are powers of 2

And if you have n that is not a power of 2 you have to find the closest
lesser power of 2 to it and go that much numbers back and sum that number
with the closest lesser power of 2.
-}
getNthSevenlikeNum :: Int -> Int
getNthSevenlikeNum n
    | n <= 0 = error "The number must be postive!"
    | n == 1 = 1
    | otherwise = if isPowerOfTwo n 
    then 7^(logBaseTwo n)
    else getNthSevenlikeNum (n - floorToPowerOfTwo n) + getNthSevenlikeNum (floorToPowerOfTwo n)i