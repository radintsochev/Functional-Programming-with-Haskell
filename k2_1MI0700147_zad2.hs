import Data.Char

main :: IO()
main = do
    print $ solve ["abode","ABc","xyzD"] == [4, 3, 1]
    print $ solve ["abide","ABc","xyz"] == [4,3,0]
    print $ solve ["IAMDEFANDJKL","thedefgh","xyzDEFghijabc"] == [6,5,7]
    print $ solve ["encode","abc","xyzD","ABmD"] == [1,3,1,3]



alphabetOrder :: [(Int, Char)]
alphabetOrder = zip [0 ..] ['a' .. 'z']

isInList :: (Eq a) => [a] -> (a -> Bool)
isInList xs = (\x -> elem x xs)

evaluateString :: String -> Int
evaluateString = length . (filter (isInList alphabetOrder)) . (zip [0 ..]) . (map toLower)

solve :: [String] -> [Int]
solve = map evaluateString