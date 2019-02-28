import Data.List.Split
import Data.List
import Data.Char
import qualified Data.Map.Strict as Map

main = do
    input <- getContents
    let parsedIn = map parseLine $ lines input
        graph = foldr (\ x acc -> insertInGraph x acc) Map.empty parsedIn
    putStrLn "All steps (first star)"
    putStrLn $ getAllSteps graph
    putStrLn "Time (first star)"
    -- putStrLn . show . workUntilAllTasksAreDone $ WorkPackage graph [] 0
    putStrLn $ show $ startWorkAndGiveTime graph

data Pair = Pair {parent :: Char, child :: Char} deriving (Show, Eq, Ord)
data PairList = PairList {parentList :: [Char], childList :: [Char]} deriving (Show, Eq, Ord)
data WorkPackage = WorkPackage {todo :: MyGraph,inProgress :: [WorkNode], timeSpent :: Int} deriving (Show, Eq)
type Node = (Char, PairList)
type WorkNode = (Char, Int) -- contains the node and the amount of work executed on it
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

getTimeForStep :: Char -> Int
getTimeForStep s = (ord s) - (ord 'A') + 61

startWorkAndGiveTime :: MyGraph -> Int
startWorkAndGiveTime g = timeSpent $ workUntilAllTasksAreDone $ WorkPackage g [] 0


workUntilAllTasksAreDone :: WorkPackage -> WorkPackage
workUntilAllTasksAreDone wp = if (todo wp == Map.empty)
                                 then wp
                                 else workUntilAllTasksAreDone $! workUntilOneTaskIsDone wp

workUntilOneTaskIsDone :: WorkPackage -> WorkPackage
workUntilOneTaskIsDone (WorkPackage g w t) = WorkPackage (removeFromGraph itemDone g) workLeft (t + time)
    where
        updatedTasks = Map.keys $ Map.filter (\l -> parentList l == "") g :: [Char]
        insertIfnew :: Char -> [WorkNode] -> [WorkNode]
        insertIfnew k acc = if elem k (fst $ unzip acc) then acc else (k, getTimeForStep k):acc
        updatedWork = sortBy (\a b -> compare (snd a) (snd b)) $ foldr insertIfnew w updatedTasks :: [WorkNode]
        time = snd $ head updatedWork
        itemDone = fst $ head updatedWork
        workTodo = take 5 updatedWork
        workNotDone = drop 5 updatedWork
        workLeft = (filter (\ (x,y) -> y /= 0) $ map (\ (x,y) -> (x,y-time)) $ workTodo) ++ workNotDone
