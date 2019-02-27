import Data.List.Split
import qualified Data.Map.Strict as Map

main = do
    input <- getContents
    let parsedIn = map parseLine $ lines input
        graph = foldr (\ x acc -> insertInGraph x acc) Map.empty parsedIn
        deptMap = Map.mapWithKey (\key x -> getDept graph key) graph
    putStrLn "All steps (first star)"
    putStrLn $ show $ getAllSteps graph

data Pair = Pair {parent :: Char, child :: Char} deriving (Show, Eq, Ord)
data PairList = PairList {parentList :: [Char], childList :: [Char]} deriving (Show, Eq, Ord)
type Node = (Char, PairList)
type MyGraph = Map.Map Char PairList

addParent :: Char -> PairList -> PairList
addParent new (PairList p c) = PairList (new:p) c

addChild :: Char -> PairList -> PairList
addChild new (PairList p c) = PairList p (new:c)

parseLine :: String -> Pair
parseLine i =
    let splInp = splitOn " must be finished before step " i
        p = last $ head splInp
        c = head $ last splInp
     in Pair p c

insertInGraph :: Pair -> MyGraph -> MyGraph
insertInGraph p g
  | (Map.member (child p) g) && (Map.member (parent p) g) = adjustChild $ adjustParent g
  | (Map.member (child p) g) && (Map.notMember (parent p) g) = adjustChild $ insertParent g
  | (Map.notMember (child p) g) && (Map.member (parent p) g) = insertChild $ adjustParent g
  | (Map.notMember (child p) g) && (Map.notMember (parent p) g) = insertChild $ insertParent g
  where
      insertParent = Map.insert (parent p) (PairList [] [child p])
      adjustParent = Map.adjust (addChild $ child p) (parent p)
      insertChild = Map.insert (child p) (PairList [parent p] [])
      adjustChild = Map.adjust (addParent $ parent p) (child p)

removeFromGraph :: Char -> MyGraph -> MyGraph
removeFromGraph c g = Map.delete c $ Map.map (removeRef c) g
    where
        removeRef :: Char -> PairList -> PairList
        removeRef a b = PairList (filter (/= a) $ parentList b) (filter (/= a) $ childList b)

getFirstStep :: MyGraph -> Char
getFirstStep g = fst $ Map.findMin $ Map.filter (\l -> parentList l == "") g

getAllSteps :: MyGraph -> String
getAllSteps g = getAllSteps' g []

getAllSteps' :: MyGraph -> String -> String
getAllSteps' g s = if (g == Map.empty) then reverse s else getAllSteps' (removeFromGraph f g) (f:s)
    where
        f = getFirstStep g

-- not used but was fun to write
getDept :: MyGraph -> Char -> Int
getDept g 'a' = maxBound -- non existing node
getDept g k = 1 + minOrZero (map (getDept g) p)
    where
        p = parentList (Map.findWithDefault (PairList ['a'] []) k g)
        minOrZero [] = 0
        minOrZero l = minimum l
