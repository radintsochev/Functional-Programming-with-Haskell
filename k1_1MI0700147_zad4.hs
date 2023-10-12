prefixToSuffix :: (Num a) => [a] -> [a]
prefixToSuffix xs = init (helper  xs [] 0)
 where
    helper :: (Num a) => [a] -> [a] -> Int -> [a]
    helper xs ys curr = if curr == length xs then (last xs) : ys else helper xs  (((last xs) - xs!!(length xs - curr - 1)  ):ys) (curr+1)