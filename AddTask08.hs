main :: IO()
main = do
    print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Bulgaria"

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City] 

getElevation :: City -> Elevation
getElevation (City _ elevation _) = elevation

getName :: Country -> Name
getName (Country name _ _) = name

isName :: Name -> City -> Bool
isName name (City cityName _ _) = name == cityName

getCapitalElevation :: Country -> Elevation 
getCapitalElevation (Country _ capital cities) = head [capitalElevation | (capitalElevation, isItACapital) <-zip (map getElevation cities)  (map (isName capital) cities), isItACapital == True] 

highestCapital :: [Country] -> Name
highestCapital countries = head [countryName | (countryName, elevation) <- zip (map getName countries) (map getCapitalElevation countries), elevation == maximum (map getCapitalElevation countries)]