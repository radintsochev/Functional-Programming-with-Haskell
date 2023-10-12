main :: IO()
main = do
    print $ minDepthGreenNode colorTree == 2

data Color = Red | Green | Blue deriving (Eq)
data Tree = Nil | Node Color Tree Tree

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Nil Nil) Nil)
                      (Node Red (Node Blue (Node Green Nil Nil)
                                           (Node Red Nil Nil))
                                Nil)

hasGreen :: Tree -> Bool
hasGreen Nil = False
hasGreen (Node value left right) = value == Green || (hasGreen left) || (hasGreen right)

minDepthGreenNode :: Tree -> Integer
minDepthGreenNode Nil = 0
minDepthGreenNode (Node value left right) = if value == Green then 0 else 1 + minimum (map minDepthGreenNode(filter hasGreen [left, right]))