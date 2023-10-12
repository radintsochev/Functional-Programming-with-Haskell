main :: IO()
main = do 
    print $ filterTypical ["Mallard", "Hook Bill", "African", "Crested","Pilgrim", "Toulouse", "Blue Swedish"] == ["Mallard", "Hook Bill","Crested", "Blue Swedish"]
    print $ filterTypical ["Mallard", "Barbary", "Hook Bill", "Blue Swedish","Crested"] == ["Mallard", "Barbary", "Hook Bill", "Blue Swedish","Crested"]
    print $ filterTypical ["African", "Roman Tufted", "Toulouse", "Pilgrim", "Steinbacher"] == []
    
typical :: [String]
typical = ["African", "Roman Tufted", "Toulouse", "Pilgrim", "Steinbacher"]

isTypical :: String -> Bool
isTypical w = w `elem` typical

filterTypical :: [String] -> [String]
filterTypical = filter (not . isTypical)