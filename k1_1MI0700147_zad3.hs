numContentChildren :: [Int] -> [Int] -> Int
numContentChildren gs ss = length$filter (==True)$zipWith (<=) gs ss