main :: IO()
main = do
    print $ maxSumSubT t1 == 5
    print $ maxSumSubT t2 == 2

data BTree a = Nil | Node a (BTree a) (BTree a)

t1 :: (Num a) => BTree a
t1 = Node 3 (Node 0 Nil Nil) (Node 2 (Node 0 Nil Nil) Nil)

t2 :: (Num a) => BTree a
t2 = Node (-3) (Node 0 Nil Nil) (Node 2 (Node 0 Nil Nil) Nil)

treeToList :: BTree a -> [a]
treeToList Nil = []
treeToList (Node value left right) = [value] ++ (treeToList left) ++ (treeToList right)

maxSumSubT :: (Num a, Ord a) => BTree a -> a
maxSumSubT Nil = 0
maxSumSubT tree = if null (filter (< 0) (treeToList tree)) then sum (treeToList tree) else sum (filter (>= 0) (treeToList tree))