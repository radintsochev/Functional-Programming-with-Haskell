import Data.List

main :: IO()
main = do
    print $ isPrimeDictionary t1 vocabulary == False
    print $ isPrimeDictionary t2 vocabulary == False
    print $ isPrimeDictionary t3 vocabulary == True

type Vocabulary = [String]
data BTree = Nil | Node Char BTree BTree deriving (Show)

t1 :: BTree
t1 = Node 'a' (Node 't' (Node 'l' (Node 't' Nil Nil) (Node 'h' Nil Nil)) (Node 'i' (Node 'e' Nil Nil) (Node 'l' Nil Nil))) (Node 'h' (Node 's' (Node 'i' Nil Nil) (Node 'S' Nil Nil)) (Node 'a' (Node 't' Nil Nil) (Node 'S' Nil Nil)))

t2 :: BTree
t2 = Node 'a' (Node 't' (Node 'l' (Node 't' Nil Nil) (Node 'h' Nil Nil)) (Node 'i' (Node 'e' Nil Nil) (Node 'l' Nil Nil))) (Node 'h' (Node 's' (Node 'i' Nil Nil) (Node 's' Nil Nil)) (Node 'p' (Node 'p' Nil Nil) (Node 'S' Nil Nil)))

t3 :: BTree
t3 = Node 'a' (Node 't' (Node 'l' Nil Nil) (Node 'i' Nil Nil)) (Node 'h' (Node 's' Nil Nil) (Node 'p' Nil Nil))

vocabulary :: Vocabulary
vocabulary = ["the", "a", "Some", "swimming", "liStS", "lisp"]

numberOfLevels :: BTree -> Int
numberOfLevels (Nil) = 0
numberOfLevels (Node _ left right) = 1 + max (numberOfLevels left) (numberOfLevels right)

traverseBFS :: BTree -> [Char]
traverseBFS tree = concat $ takeWhile (not . null) $ map (getLevel tree) [0 .. ]

getLevel :: BTree -> Int -> [Char]
getLevel Nil _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

isPrime :: Int -> Bool
isPrime n = null [d | d <- [2 .. (n - 1)], mod n d == 0]

isPrimeDictionary :: BTree -> Vocabulary -> Bool
isPrimeDictionary tree vocab = isPrime (sum [level + length v| (word, level) <- zip (map (getLevel tree) [0 .. ((numberOfLevels t1) - 1)]) [0 ..], v <- vocab, isInfixOf v word])