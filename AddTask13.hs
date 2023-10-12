main :: IO()
main = do
    print $ findJudge 2 [(1, 2)] == 2
    print $ findJudge 3 [(1, 3), (2, 3)] == 3
    print $ findJudge 3 [(1, 3), (2, 3), (3, 1)] == -1
    print $ findJudge 3 [(1, 2), (2, 3)] == -1
    print $ findJudge 4 [(1, 3), (1, 4), (2, 3), (2, 4), (4, 3)] == 3

trustNumber :: Int -> [(Int, Int)] -> Int
trustNumber x relations = length [(f, s) | (f, s) <- relations, x == s]

trustingNumber :: Int -> [(Int, Int)] -> Int
trustingNumber x relations = length [(f, s) | (f, s) <- relations, x == f]

isJudge :: Int -> [(Int, Int)] -> Int -> Bool
isJudge numberOfPeople relations x = ((trustNumber x relations) == numberOfPeople - 1) && ((trustingNumber x relations) == 0)

findJudge :: Int -> [(Int, Int)] -> Int
findJudge numberOfPeople relations = if null [j | j <- [1 .. numberOfPeople], isJudge numberOfPeople relations j] then -1 else head [j | j <- [1 .. numberOfPeople], isJudge numberOfPeople relations j]