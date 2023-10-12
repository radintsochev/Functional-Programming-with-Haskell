main :: IO()
main = do
    print $ minDepthGreenNode colorTree == 3

data Color = Red | Green | Blue deriving (Show, Eq)
data Tree = Empty | Node Color Tree Tree

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Blue Empty Empty) Empty)
                      (Node Red (Node Blue (Node Green Empty Empty)
                                           (Node Red Empty Empty))
                                Empty)

getLevel :: Tree -> Int -> [Color]
getLevel Empty _ = []
getLevel (Node value _ _) 0 = [value]
getLevel (Node value left right) k = getLevel left (k - 1) ++ getLevel right (k - 1)

minDepthGreenNode :: Tree -> Int
minDepthGreenNode t = helper 0
 where
    helper :: Int -> Int
    helper k
     | null $ getLevel t k = -1
     | elem Green $ getLevel t k = k
     | otherwise = helper $ k + 1