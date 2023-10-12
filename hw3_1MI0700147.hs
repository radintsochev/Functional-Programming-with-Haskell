main::IO()
main = do
    print $ prune (T 1 [T 2 [T 3 []], T 4 [T 5 [T 6 []]], T 7 [T 8 [], T 9 [T 10 [T 11 []]]]]) == (T 1 [T 2 [T 3 []], T 4 [T 5 []], T 7 [T 8 [], T 9 [T 10 []]]])

data NTree a = T a [(NTree a)]
 deriving (Show, Eq)

isStick :: NTree a -> Bool
isStick (T a []) = True
isStick (T  a (x:xs)) = length (x:xs) <= 1 && and (map isStick (x:xs))


removeLast :: NTree a -> NTree a
removeLast (T a []) = (T a [])
removeLast (T a [(T b [])]) = (T a [])
removeLast (T a [x]) = (T a [removeLast x])
removeLast (T a (x:xs)) = (T a (map removeLast (xs)))

cut :: NTree a -> NTree a
cut (T a []) = (T a [])
cut (T a [(T b [])]) = (T a [(T b [])])
cut tree@(T a [x])
 | isStick tree = cut $ removeLast (T a [x])
 | otherwise = cut x
cut (T a (x:xs)) = (T a (map cut (x:xs)))

prune :: NTree a -> NTree a
prune (T a (xs)) = T a (map cut (xs))