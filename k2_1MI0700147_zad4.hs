controller :: String -> String
controller (x:xs) = replicate (length (x:xs)) '0'