import Data.Time
import Data.List.Split
import qualified Data.Map.Strict as Map

-- Needs sorted imput, can easily be sorted usinf "sort" on unix systems.
main = do
    input <- getContents
    let parsedIn = parseInput [] (lines  input) (-1)
        minuteMap = createMinuteMap parsedIn
        sleepingGuard = fst $ getMostSleep $ Map.toList $ minuteMap
        sleepingMinute = fst $ getMostCommonMinute (Map.findWithDefault [-1] sleepingGuard minuteMap)
        guardWithMostSleptMinute = getGuardAndMostSleptMinute $ map listMostCommonMinute $ Map.toList $ minuteMap
    putStrLn "Guard:"
    putStrLn $ show $ sleepingGuard
    putStrLn "Minute:"
    putStrLn $ show $ sleepingMinute
    putStrLn "Multiplication (solution for first star):"
    putStrLn $ show $ sleepingGuard * sleepingMinute
    putStrLn "Guard and most slept minute and times this minute was aslept:"
    putStrLn $ show $ guardWithMostSleptMinute
    putStrLn "Multiplication (solution for second star):"
    putStrLn $ show $ (fst guardWithMostSleptMinute) * (fst $ snd guardWithMostSleptMinute)


data SleepLog = SleepLog { sleep :: Maybe UTCTime, awake :: Maybe UTCTime, idGuard :: Int } deriving (Show)

getMinute :: UTCTime -> Int
getMinute tm =  read $ formatTime defaultTimeLocale "%-M" tm

getMinutes :: SleepLog -> [Int]
getMinutes sl = case ((sleep sl), (awake sl)) of ((Just s), (Just a)) -> [(getMinute s) .. ((getMinute a)-1)]
                                                 otherwise -> [(-1)]

addToMinuteMap :: Map.Map Int [Int] -> SleepLog -> Map.Map Int [Int]
addToMinuteMap a s = case a Map.!? (idGuard s) of
                            Nothing -> Map.insert (idGuard s) (getMinutes s) a
                            Just x -> Map.adjust ((getMinutes s) ++) (idGuard s) a

createMinuteMap :: [SleepLog] -> Map.Map Int [Int]
createMinuteMap a = foldl (\acc x -> addToMinuteMap acc x) Map.empty a

--                id   max
getMostSleep :: [(Int, [Int])] -> (Int, Int)
getMostSleep a = foldl (\(idGuard, minutesAsleep) (idX, listMinX) ->
                   if length listMinX > minutesAsleep
                      then (idX, length listMinX)
                      else (idGuard, minutesAsleep))
                          (-1,-1)
                          a

--                     minutes -> (mostCommonMinute, sizeofMostCommonMinute)
getMostCommonMinute :: [Int] -> (Int, Int)
getMostCommonMinute i = foldl (\(mi,s) x ->  if length (filter (==x) i) > s
                                             then (x, length $ filter (==x) i)
                                             else (mi,s))
                              (0,0)
                              i

listMostCommonMinute :: (Int, [Int]) -> (Int, (Int, Int))
listMostCommonMinute (idGuard, minutes) = (idGuard, getMostCommonMinute minutes)

--                             [(id,  (minute,size))] -> id , 
getGuardAndMostSleptMinute :: [(Int, (Int, Int))] -> (Int, (Int, Int))
getGuardAndMostSleptMinute i = foldl (\(idAcc, (miAcc,sAcc)) (idX, (miX,sX)) -> 
                                      if sX > sAcc
                                          then (idX, (miX,sX))
                                          else (idAcc, (miAcc,sAcc)))
                                     (0,(0,0))
                                     i



parseInput :: [SleepLog] -> [String] -> Int -> [SleepLog]
parseInput log l id
    | l == [] = log
    | isHeader (head $ l) = parseInput log (tail l) $ getId $ head l
    | isSleep (head $ l) = parseInput (log ++ [SleepLog (getTime (head l)) (getTime (head (tail l))) id])
                                    (tail (tail l))
                                    id

getTime :: String -> Maybe UTCTime
getTime i = parseTimeM True defaultTimeLocale "[%Y-%-m-%-d %-R" (head (splitOn "]" i))

getId :: String -> Int
getId i = read $ head $ splitOn " " $ head $ tail $ splitOn "#" i

isHeader :: String -> Bool
isHeader i = case head $ tail ( splitOn "] " i) of ('G':'u':'a':_) -> True
                                                   otherwise -> False

isSleep :: String -> Bool
isSleep i = if head (tail ( splitOn "] " i)) == "falls asleep" then True else False



