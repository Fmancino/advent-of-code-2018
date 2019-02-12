import Data.List.Split
import qualified Data.Map.Strict as Map

main = do
    input <- getContents
    putStrLn("How many overlaps:")
    let listOfSquares = getListOfSquares input
        mapofsuit = pushListToSquareInchMap Map.empty $ concat $ listOfSquares
    putStrLn $ show $ countSquareInchesWithOverlap $ mapofsuit
    putStrLn("Id of not overlapping:")
    putStrLn $ isAnyListMemberOfMap (getNonOverlapping $ mapofsuit) (listOfSquares)

data Point = Point {xcoord :: Int, ycoord :: Int} deriving (Show, Eq, Ord)

pushElemToSqareInchSeq :: Map.Map Point [Int] -> (Point,[Int]) -> Map.Map Point [Int]
pushElemToSqareInchSeq a (p,v) = case a Map.!? p of
                            Nothing -> Map.insert p v a
                            Just x -> Map.adjust (v ++) p a

pushListToSquareInchMap :: Map.Map Point [Int] -> [(Point,[Int])] -> Map.Map Point [Int]
pushListToSquareInchMap a [] = a
pushListToSquareInchMap a b = pushListToSquareInchMap (pushElemToSqareInchSeq a (head b)) (tail b)

convertEntryToSquareInches :: [Int] -> [(Point,[Int])]
convertEntryToSquareInches (id:xn:yn:l:h:end) = [((Point x y), [id]) | x <- [xn..(xn+l-1)], y <- [yn..(yn+h-1)]]

getEntryAsInts :: String -> [Int]
getEntryAsInts s = map (read . Prelude.filter (/=' ')) $ tail $ splitOneOf "#@,:x" s

countSquareInchesWithOverlap :: Map.Map Point [Int] -> Int
countSquareInchesWithOverlap a = Map.size (Map.filter (\ x -> length x > 1) a)

getNonOverlapping :: Map.Map Point [Int] -> Map.Map Point [Int]
getNonOverlapping a = Map.filter (\ x -> length x == 1) a

isListMemberOfMap :: Bool -> Map.Map Point [Int] -> [(Point,[Int])] -> Bool
isListMemberOfMap False _ _ = False
isListMemberOfMap True _ [] = True
isListMemberOfMap True a b = if Map.member (fst (head b)) a then isListMemberOfMap True a $ tail b else False

isAnyListMemberOfMap :: Map.Map Point [Int] -> [[(Point,[Int])]] -> String
isAnyListMemberOfMap _ [] = "No list Found"
isAnyListMemberOfMap a b = if isListMemberOfMap True a (head b)
                           then show $ head $ snd $ head $ head b
                           else isAnyListMemberOfMap a $ tail b

getListOfSquares :: String -> [[(Point,[Int])]]
getListOfSquares s = map (convertEntryToSquareInches . getEntryAsInts) $ lines s
