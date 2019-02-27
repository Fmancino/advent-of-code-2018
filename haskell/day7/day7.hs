import Data.List.Split
import qualified Data.Map.Strict as Map

main = do
    input <- getContents
    let parsedIn = map parseLine $ lines input
    putStrLn "Input"
    putStrLn $ show $ parsedIn

data Pair = Pair {parent :: Char, child :: Char} deriving (Show, Eq, Ord)

parseLine :: String -> Pair
parseLine i =
    let splInp = splitOn " must be finished before step " i
        p = last $ head splInp
        c = head $ last splInp
     in Pair p c
