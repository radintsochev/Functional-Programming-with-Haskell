data BTree = Nil | Node Int BTree BTree

t1 :: BTree                                 
t1 = Node 5 Nil (Node 4 (Node 5 Nil Nil) (Node 7 Nil Nil))

t2 :: BTree                                 
t2 = Node 5 (Node 3 Nil Nil) (Node 4 (Node 5 Nil Nil) (Node 7 Nil Nil))

treeHeight :: BTree -> Int
treeHeight Nil = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

isBalanced :: BTree -> Bool
isBalanced Nil = True
isBalanced (Node _ left right) = (max (treeHeight left) (treeHeight right)) - (min (treeHeight left) (treeHeight right)) <=  1 && isBalanced left && isBalanced right