import Data.List.Split
import Data.Foldable
import qualified Data.Map.Strict as Map

main = do
    input <- getContents
    let parsedIn = map parsePoint $ lines input
    putStrLn $ show $ parsedIn

data Point = Point {xcoord :: Int, ycoord :: Int} deriving (Show, Eq, Ord)

parsePoint :: String -> Point
parsePoint i = let splInp = splitOn ", " i
               in Point (read $ head splInp) (read $ last splInp)

getArea :: [Point] -> [Point]
getArea i = [Point x y | x <- [minX i .. maxX i], y <- [minY i .. maxY i]]

maxY :: [Point] -> Int
maxY i = foldl' (\acc x -> if ycoord x > acc then ycoord x else acc) 0 i

minY :: [Point] -> Int
minY i = foldl' (\acc x -> if ycoord x < acc then ycoord x else acc) 0 i

maxX :: [Point] -> Int
maxX i = foldl' (\acc x -> if xcoord x > acc then xcoord x else acc) 0 i

minX :: [Point] -> Int
minX i = foldl' (\acc x -> if xcoord x < acc then xcoord x else acc) 0 i

distance :: Point -> Point -> Int
distance a b = (abs (xcoord a - xcoord b)) + (abs $ ycoord a - ycoord b)
