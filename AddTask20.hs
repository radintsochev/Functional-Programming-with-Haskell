main :: IO()
main = do
    print $ (averageFunction [(+1), (**0.5), (2**)]) 2 == 2.8047378541243653

averageFunction :: (Num a, Fractional a) => [(a -> a)] -> (a -> a)
averageFunction fs = (\x -> (sum $ map (\f -> f x) fs) / (fromIntegral $ length fs))