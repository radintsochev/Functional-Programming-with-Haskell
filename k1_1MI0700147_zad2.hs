fibbNum :: Int -> Int
fibbNum n = helper n 0 0
 where
    helper :: Int -> Int -> Int
    helper n n+1 result = result
    helper n 0 result = helper n 1 result + 0
    helper n 1 result = helper n 2 result + 1
    helper n curr result = helper n curr+1 result+