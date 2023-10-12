main :: IO()
main = do
    print $ coldestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Germany"

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City] 

myCountry :: Country
myCountry = Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]

getAvg :: Country -> Double
getAvg (Country _ _ cities) = (sum temperatures) / (fromIntegral (length temperatures))
 where
    temperatures = [temperature | (City _ _ temperature) <- cities]

getName :: Country -> Name
getName (Country name _ _) = name

coldestCapital :: [Country] -> Name
coldestCapital countries =  head [name | (name, avg) <- (zip (map getName countries) (map getAvg countries)), avg == minimum (map getAvg countries)]