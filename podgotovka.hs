numberOfDivisors :: Int -> Int
numberOfDivisors x = length [m | m  <- [1..x], x `rem` m == 0]

isNumberOfDivisorsEven :: Int -> Bool
isNumberOfDivisorsEven x = (numberOfDivisors x) `rem` 2 == 0

sumOfEvenly :: Int -> Int -> Int
sumOfEvenly x y = sum [m | m <- [x..y], isNumberOfDivisorsEven m]

hasAtLeastOneElementPM :: [Int] -> Bool
hasAtLeastOneElementPM [] = False
hasAtLeastOneElementPM _ = True

hasAtLeastOneElementFunc :: [Int] -> Bool
hasAtLeastOneElementFunc (xs) = length (xs) > 0

myLengthRecPM :: [Int] -> Int
myLengthRecPM [] = 0
myLengthRecPM (x:xs) = 1 + myLengthRecPM (xs)

getClosedIntervalRec :: Int -> Int -> [Int]
getClosedIntervalRec x y
    | x > y = (y: getClosedIntervalRec (y + 1) x)
    | x < y = (x: getClosedIntervalRec (x + 1) y)
    | x == y = [x]

getClosedIntervalOneLine :: Int -> Int -> [Int]
getClosedIntervalOneLine x y = [min x y .. max x y]

isInside :: Int -> Int -> Int -> Bool
isInside x y z = elem z (getClosedIntervalOneLine x y)

removeFirst :: Int -> [Int] -> [Int]
removeFirst x (ys) = if x == head (ys) then tail (ys) else [head (ys)] ++ removeFirst x (tail (ys))

removeAllListC :: Int -> [Int] -> [Int]
removeAllListC x (ys) = [m | m <- (ys), m /= x]

removeAllHOF :: Int -> [Int] -> [Int]
removeAllHOF d = filter (/= d)

incrementByLC :: Int -> [Int] -> [Int]
incrementByLC x (ys) = [m + x | m <- (ys)]

incrementByHOF :: Int -> [Int] -> [Int]
incrementByHOF d = map (+d)

rev :: Int -> Int
rev = read . reverse . show

fact :: Int -> Int
fact x = product [1.. x]

isPrime :: Int -> Bool
isPrime n = n > 1 && null [d | d <- [2 .. n - 1], mod n d == 0]

myLambda :: (a -> a) -> a -> a
myLambda f x = f x

negatePred :: (a -> Bool) -> a -> Bool
negatePred f x = if (f x) then False else True

compose :: (a -> a) -> (a -> a) -> a -> a
compose f g x = f (g x)

partiallyApply :: (a -> a -> a) -> a -> a -> a 
partiallyApply f x y = f x y

difference :: (Num a) => (a -> a) -> (a -> a -> a)
difference f =  (\x y -> f y - f x)

betterFunc :: (Ord a) => (a -> a) -> a -> (a -> a)
betterFunc f x = (\y -> max (f y) x) 

sumTupleNonPM :: (Int, Int) -> Int
sumTupleNonPM tuple = fst tuple + snd tuple

type Point = (Int, Int)

divideNonPM :: Point -> Point
divideNonPM (x, y) = (x `div` y, x `mod` y)

type Rat = (Int, Int)

normalize :: Rat -> Rat
normalize (x, y) = (x `div` z, y `div` z)
 where
    z = gcd x y
getSquares :: Int -> Int -> Int -> [(Int, Int)]
getSquares x y z = [m | m <- [x, (x + z) .. y]] `zip` [m^2 | m <- [x, (x + z) .. y]]

type Vector = (Int, Int, Int)

sumVectors :: Vector -> Vector -> Vector
sumVectors (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

scalVectors :: Vector -> Int -> Vector
scalVectors (x, y , z) a = (x * a, y * a, z * a)

getPrimesLC :: Int -> Int -> [Int]
getPrimesLC x y = [m | m <- [x..y], length [n | n <- [2..m-1], m `mod` n == 0] == 0, elem '7' (show m)]

isArithmetic :: [Int] -> Bool
isArithmetic (x:xs) = all ( == (x - head xs)) (listOfDiff xs)
 where
    listOfDiff :: [Int] -> [Int]
    listOfDiff xs = zipWith (-) xs (tail xs)

applyN :: (Num a) => (a -> a) -> a -> a -> a
applyN f 1 x = f x
applyN f n x = f . (applyN f n-1 x) x