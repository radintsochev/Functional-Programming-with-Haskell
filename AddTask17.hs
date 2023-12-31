main :: IO()
main = do
    print $ isBoring t1 == False
    print $ isBoring t2 == True

data NTree a = Nil | Node a [(NTree a)]

t1 :: (Num a) => NTree a
t1 = Node 10 [Node 10 [Node 10 [Nil], Node 8 [Node 10 [Nil]], Node 2 [Nil]], Node 10 [Node 11 [Nil], Node 10 [Nil], Node 6 [Nil]]]

t2 :: NTree Char
t2 = Node 's' [Node 's' [Node 's' [Nil], Node 's' [Node 's' [Nil]]]]

treeToList :: NTree a -> [a]
treeToList Nil = []
treeToList (Node value trees) = foldl (++) [value] (map treeToList trees)

isBoring :: (Eq a) => NTree a -> Bool
isBoring (Node value trees) = length (filter (== value) (treeToList (Node value trees))) == length (treeToList (Node value trees))