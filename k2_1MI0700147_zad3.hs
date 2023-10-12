import Data.List

main :: IO()
main = do
    print $ mapLevel t1 [(*2), (*4), (`div` 100)] == t11Result
    print $ mapLevel t1 [show, (nub . show . (* 1000))] == t12Result
    print $ mapLevel t2 [(\ _ -> "r"), (\ char -> "w_" ++ [char]), (\ char -> "l_" ++ [char])] == t2Result

data NTree a = Nil | Node a [NTree a] deriving (Eq, Show)

t1 :: NTree Int
t1 = Node 10 [Node 10 [Node 10 [], Node 8 [Node 10 []], Node 2 []], Node 10 [Node 11 [], Node 10 [], Node 6 []]]

t2 ::  NTree Char
t2 = Node 's' [Node 's' [Node 's' [], Node 's' []]]

t11Result :: NTree Int
t11Result = Node 20 [Node 40 [Node 0 [],Node 0 [],Node 0 []],Node 40 [Node 0 [],Node 0 [],Node 0 []]]

t12Result :: NTree String
t12Result = Node "10" [Node "10" [],Node "10" []]

t2Result :: NTree String
t2Result = Node "r" [Node "w_s" [Node "l_s" [],Node "l_s" []]]

mapLevel :: NTree a -> [(a -> b)] -> NTree b
mapLevel Nil _  = Nil
mapLevel (Node value trees) [f] = (Node (f value) [])
mapLevel (Node value trees) (f:fs) = (Node (f value) (map (\t -> mapLevel t fs) trees))
