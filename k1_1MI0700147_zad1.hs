numDrink :: Int -> Int -> Int
numDrink x y = helper x 0 y
 where
    helper :: Int -> Int -> Int -> Int
    helper x f y =  if x + f < y then x else if x == 0 then helper (f `div` y) (f `mod` y) y else x + helper (x `div` y) (f + (x `mod` y)) y