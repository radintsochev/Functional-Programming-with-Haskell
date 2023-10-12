main :: IO()
main = do
    print $ cP [Present, Late, Present, Absent, Present, Present, Present, Absent] == False
    print $ cP [Present, Late, Present, Late, Present, Late, Present, Absent, Late, Present] == True
    print $ cP [Present, Late, Present, Late, Late, Late, Present, Present, Absent, Present] == False

type Misses = Int
type Lates = Int
type Criterion = (Misses, Lates)
data Attendance = Absent | Late | Present deriving (Eq, Show)
type StudentRecord = [Attendance]

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

cP = canPass (1, 2)

helper :: [Attendance] -> Int -> [Attendance] 
helper list size = (if size > (length list) then [] else (head list : (take (size - 1) (tail list))) ++ (helper (tail list) size)) 

getSubLists :: [Attendance] -> Int -> [[Attendance]]
getSubLists list size = splitEvery size (helper list size)

canPass :: Criterion -> (StudentRecord -> Bool)
canPass (maxA, maxL) = (\x -> (length (filter (== Absent) x)) <= maxA && ((replicate (maxL + 1) Late) `notElem` (getSubLists x (maxL + 1))))