import Data.List (sort, find)

main = do
    input <- getContents
    putStr("Exercize a:\n")
    putStr(showChecksum input ++ "\n")
    putStr("Exercize b:\n")
    putStr(showSimilarStrings input ++ "\n")
    putStr(showSimilarPart (showSimilarStrings input) ++ "\n")


showChecksum :: String -> String
showChecksum i = let
    sortedStrings = map sort $ lines i
    bools = map checkOneArg sortedStrings
    in show $ (sumTwo bools) * (sumTree bools)

sumTwo :: [(Bool, Bool)] -> Int
sumTwo i = sum $ map (boolToInt . fst) i

sumTree :: [(Bool, Bool)] -> Int
sumTree i = sum $ map (boolToInt . snd) i

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

checkOneArg :: String -> (Bool, Bool)
checkOneArg i = checkSortedString i (False, False)

checkSortedString :: String -> (Bool, Bool) -> (Bool, Bool)
checkSortedString "" (a, b) = (a, b)
checkSortedString _ (True, True) = (True, True)
checkSortedString i (a, b)
        | (length i > 3) && (h == i!!1) && (h == i!!2) && (h == i!!3) = checkSortedString (filter (/=h) i) (a, b)
        | (length i >= 3) && (h == i!!1) && (h == i!!2) = checkSortedString (filter (/=h) i) (a, True)
        | (length i >= 2) && (h == i!!1) = checkSortedString (filter (/=h) i) (True, b)
        | otherwise = checkSortedString (filter (/=h) i) (a, b)
    where h = head i

isSimilar :: String -> String -> Bool
isSimilar a b = let
    c = zip a b
    in (sum $ map (boolToInt . isDifferent) c) == 1

isDifferent :: (Char, Char) -> Bool
isDifferent c = fst c /= snd c

getSimilarStrings :: [String] -> (String, String)
getSimilarStrings (h:xs) = case find (isSimilar h) xs of
        Nothing -> getSimilarStrings xs
        Just x -> (h, x)

showSimilarStrings :: String -> String
showSimilarStrings i = let sim = getSimilarStrings $ lines i in (fst sim) ++ "\n" ++ (snd sim)

getSimilarPart :: (String, String) -> String -> String
getSimilarPart ([],[]) c = c
getSimilarPart (a,b) c = if head a == head b
                         then getSimilarPart (tail a, tail b) (c ++ [head a])
                         else getSimilarPart (tail a, tail b) c

showSimilarPart :: String -> String
showSimilarPart i = getSimilarPart (getSimilarStrings (lines i)) ""
