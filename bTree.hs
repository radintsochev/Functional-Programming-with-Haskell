import Data.Char

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq) -- std::operator<<

mapTree :: BTree a -> (a -> b) -> BTree b
mapTree Nil _ = Nil
mapTree (Node value left right) f = Node (f value) (mapTree left f) (mapTree right f)

traverseBFS :: BTree a -> [a]
traverseBFS tree = concat $ takeWhile (not . null) $ map (getLevel tree) [0 .. ]

traverseBFS' tree = concat $ takeWhile (\ xs -> not $ null xs) $ map (\ k -> getLevel tree k) [0 .. ]

getLevel :: BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

traverseDFS :: BTree a -> [a]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right

sumTree :: (Num a) => BTree a -> a
sumTree Nil = 0
sumTree (Node value left right) = value + sumTree left + sumTree right

size :: BTree a -> Int
size Nil = 0
size (Node _ left right) = 1 + size left + size right

numberBTree :: BTree Int
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))



