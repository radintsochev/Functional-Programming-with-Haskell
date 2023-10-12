main :: IO()
main = do
   print $ (myPoly [2.7, 3.0 ..]) 2.2 3 == -0.4399999999999998

sub :: (Num a) => a -> (a -> a)
sub n = (\x -> n - x)

myPoly :: [Double] -> (Double -> Int -> Double)
myPoly xs = (\x y -> foldl (*) (head (take y (map (sub x) xs))) (tail (take y (map (sub x) xs))))