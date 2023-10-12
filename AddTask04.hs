main :: IO()
main = do
   print $ (getOddCompositionValue [(+1),(*2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2
   print $ (getOddCompositionValue [id, (5-), (\x -> mod x 2)]) 15 == 1


getOddCompositionValue :: (Num a) => [(a -> a)] -> (a -> a)
getOddCompositionValue fs = foldl (.) (head fs) (tail [f | (f, i) <- zip fs [1 ..], mod i 2 == 1])