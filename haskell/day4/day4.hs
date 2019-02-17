import Data.Time
import Data.List.Split
import qualified Data.Map.Strict as Map

main = do
    input <- getContents
    let parsedIn = parseInput [] (lines  input) (-1)
        minuteMap = createMinuteMap Map.empty parsedIn
        sleepingGuard = fst $ getMostSleep (-1, -1) $ Map.toList $ minuteMap
        sleepingMinute = fst $ getMostCommonMinute (-1,-1) (Map.findWithDefault [-1] sleepingGuard minuteMap)
    putStrLn "Guard:"
    putStrLn $ show $ sleepingGuard
    putStrLn "Minute:"
    putStrLn $ show $ sleepingMinute
    putStrLn "Mult:"
    putStrLn $ show $ sleepingGuard * sleepingMinute


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

createMinuteMap :: Map.Map Int [Int] -> [SleepLog] -> Map.Map Int [Int]
createMinuteMap a [] = a
createMinuteMap a b = createMinuteMap (addToMinuteMap a (head b)) (tail b)

--                id   max
getMostSleep :: (Int, Int) -> [(Int, [Int])] -> (Int, Int)
getMostSleep a [] = a
getMostSleep a b = let nrMinutes = length $ snd $ head $ b
                   in if nrMinutes > snd a
                      then getMostSleep (fst $ head b, nrMinutes) (tail b)
                      else getMostSleep a (tail b)

--                     minute size
getMostCommonMinute :: (Int, Int) -> [Int] -> (Int, Int)
getMostCommonMinute a [] = a
getMostCommonMinute (mi, s) i = if length (filter (==head i) i) > s
                                then getMostCommonMinute (head i, length $ filter (==head i) i) (filter (/=head i) i)
                                else getMostCommonMinute (mi,s) (filter (/=head i) i)




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



