import Data.List.Split
import Data.Foldable
import qualified Data.Map.Strict as Map

main = do
    input <- getContents
    let parsedIn = map parsePoint $ lines input
        areaDelim = getAreaDelim parsedIn
        area = getArea' areaDelim
        listOfClosestPoints = removeNothingPoints $ zip area $ map (getClosestPoint parsedIn) $ area
        perim = getPerimeter areaDelim listOfClosestPoints
        infinitePoints = getPoints perim
        nonInfiniteArea = filter (\x -> notElem (snd x) infinitePoints) listOfClosestPoints
        mostCommonFiniteArea = getMostCommonElementAndSize $ snd $ unzip nonInfiniteArea
    putStrLn "Most common area: (first star)"
    putStrLn $ show $ mostCommonFiniteArea

data Point = Point {xcoord :: Int, ycoord :: Int} deriving (Show, Eq, Ord)
data AreaDelim = AreaDelim {minP :: Point, maxP :: Point} deriving (Show)

getAreaDelim :: [Point] -> AreaDelim
getAreaDelim a = AreaDelim (Point (minX a) (minY a)) (Point (maxX a) (maxY a))

removeNothingPoints :: [(Point, Maybe Point)] -> [(Point, Point)]
removeNothingPoints a = foldr f [] a
       where f (_,Nothing) acc = acc
             f (a, Just x) acc = ([(a,x)] ++ acc)


getPerimeter :: AreaDelim -> [(Point, Point)] -> [(Point, Point)]
getPerimeter (AreaDelim (Point minPx minPy) (Point maxPx maxPy)) a =
         filter (\x -> xcoord (fst x) == minPx
                       || xcoord (fst x) == maxPx
                       || ycoord (fst x) == minPy
                       || ycoord (fst x) == maxPy) a

getMostCommonElementAndSize :: Ord a => [a] -> (a, Int)
getMostCommonElementAndSize x = getMostCommonElementImpl (head x, 0) x

getMostCommonElementImpl :: Ord a => (a, Int) -> [a] -> (a, Int)
getMostCommonElementImpl acc [] = acc
getMostCommonElementImpl acc i = if sizeH > snd acc
                            then getMostCommonElementImpl (h, sizeH) (filter (/=h) i)
                            else getMostCommonElementImpl acc (filter (/=h) i)
                        where h = head i
                              sizeH = length $ filter (==h) i

getPoints :: [(Point, Point)] -> [Point]
getPoints a = foldr f [] p
       where p = snd (unzip a)
             f x acc = if elem x acc then acc else (acc ++ [x])

parsePoint :: String -> Point
parsePoint i = let splInp = splitOn ", " i
               in Point (read $ head splInp) (read $ last splInp)

getArea :: [Point] -> [Point]
getArea i = [Point x y | x <- [minX i .. maxX i], y <- [minY i .. maxY i]]

getArea' :: AreaDelim -> [Point]
getArea' (AreaDelim (Point minPx minPy) (Point maxPx maxPy)) =
                  [Point x y | x <- [minPx .. maxPx], y <- [minPy .. maxPy]]

maxY :: [Point] -> Int
maxY i = foldl' (\acc x -> if ycoord x > acc then ycoord x else acc) 0 i

minY :: [Point] -> Int
minY i = foldl' (\acc x -> if ycoord x < acc then ycoord x else acc) maxBound i

maxX :: [Point] -> Int
maxX i = foldl' (\acc x -> if xcoord x > acc then xcoord x else acc) 0 i

minX :: [Point] -> Int
minX i = foldl' (\acc x -> if xcoord x < acc then xcoord x else acc) maxBound i

distance :: Point -> Point -> Int
distance a b = (abs ((xcoord a) - (xcoord b))) + (abs ((ycoord a) - (ycoord b)))

getClosestPoint :: [Point] -> Point -> Maybe Point
getClosestPoint pnts p = let !pntDist = zip pnts (map (distance p) pnts)
                         in getClosestPoint' [(Point 0 0, maxBound :: Int)] pntDist

getClosestPoint' :: [(Point, Int)] -> [(Point, Int)] -> Maybe Point
getClosestPoint' (acc:accs) [] = if accs == [] then Just (fst acc) else Nothing
getClosestPoint' acc (x:xs) = getClosestPoint' (leastOrBoth acc x) xs

leastOrBoth :: [(Point, Int)] -> (Point, Int) -> [(Point, Int)]
leastOrBoth (a:as) b
                   | snd a == snd b = (a:as) ++ [b]
                   | snd a < snd b = (a:as)
                   | snd a > snd b = [b]

