import Data.Set (Set, empty, insert, member, singleton)

main = do
    input <- getContents
    putStr("Exercize a:\n")
    putStr(showSum input ++ "\n")
    putStr("Repeat:\n")
    putStr(showRepeat input ++ "\n")


showSum :: String -> String
showSum = show . sum . getNumbersInLines

removeLeadingPlus :: String -> String
removeLeadingPlus ('+':s) = s
removeLeadingPlus s = s

getNumbersInLines :: String -> [Integer]
getNumbersInLines t = map (read . removeLeadingPlus) $ lines t

addUntilRepeat :: Integer -> Set Integer -> [Integer] -> Integer
addUntilRepeat current seen (this:tailOfList)
        | member next seen = next  -- If next is a member of see you should quit and return
        | otherwise = addUntilRepeat next (insert next seen) tailOfList
    where next = current + this

showRepeat :: String -> String
showRepeat i = let
    integerInput = cycle $ getNumbersInLines  i
    seenStuff = singleton 0
    startFreq = 0
    in show $ addUntilRepeat startFreq seenStuff integerInput

