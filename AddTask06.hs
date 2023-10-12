main :: IO()
main = do
    print $ maxDepthBlueNode colorTree == 2

data Color = Red | Green | Blue deriving (Eq)
data Tree = Nil | Node Color Tree Tree

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Nil Nil) Nil)
                      (Node Red (Node Blue (Node Green Nil Nil)
                                           (Node Red Nil Nil))
                                Nil)

hasBlue :: Tree -> Bool
hasBlue Nil = False
hasBlue (Node value left right) = value == Blue || (hasBlue left) || (hasBlue right)

maxDepthBlueNode :: Tree -> Integer
maxDepthBlueNode Nil = 0
maxDepthBlueNode (Node value left right) = if ((value == Blue) && (not $ hasBlue left) && (not $ hasBlue right)) then 0 else 1 + maximum (map maxDepthBlueNode(filter hasBlue [left, right]))