import Debug.Trace
import Data.List.Split
import Data.List
import Data.Char
import qualified Data.Map.Strict as Map

{-# LANGUAGE BangPatterns #-}
main = do
    input <- getContents
    let inputN = filter (/='\n') input  -- remove automatically added newline
        inputInt = map read $ splitOn " " inputN :: [Int]
    putStrLn $ "First star:"
    putStrLn $ show $ sumMeta inputInt
    putStrLn $ "Second star:"
    putStrLn $ show $ sumPartTwo inputInt

--      number of     child , metadata
type ChildsAndMeta = (Int, Int)
type ChildValues = [Int]
type MetaValues = [Int]
type ChildValuesAndMeta = (ChildValues, ChildsAndMeta)

sumMeta :: [Int] -> Int
sumMeta a = sumMeta' a [] 0


sumMeta' :: [Int] -> [ChildsAndMeta] -> Int -> Int
sumMeta' [] [] c = c
sumMeta' a ((0,meta):b) c = sumMeta' (drop meta a) (removeChild b) strictSum
    where !strictSum = (sum $ take meta a) + c
sumMeta' (childs:meta:a) b c = if childs > 0
                                  then sumMeta' a ((childs,meta):b) c
                                  else sumMeta' (drop meta a) (removeChild b) strictSum
    where !strictSum = (sum $ take meta a) + c

removeChild :: [ChildsAndMeta] -> [ChildsAndMeta]
removeChild [] = []
removeChild ((child,meta):as) = ((child-1),meta):as

sumPartTwo :: [Int] -> Int
sumPartTwo a = sumPartTwo' a []

sumPartTwo' :: [Int] -> [ChildValuesAndMeta] -> Int
sumPartTwo' a [(values, (0, meta))] = metaSum (reverse values) $ take meta a
sumPartTwo' a ((values, (0, meta)):b) = sumPartTwo' (drop meta a) $
    removeChildTwo b (sumPartTwo' (take meta a) [(values, (0, meta))])
sumPartTwo' (childs:meta:a) b = if childs > 0
                                then sumPartTwo' a (([],(childs,meta)):b)
                                else sumPartTwo' (drop meta a) $
                                        removeChildTwo (b) (sum $ take meta a)

removeChildTwo :: [ChildValuesAndMeta] -> Int -> [ChildValuesAndMeta]
removeChildTwo [] _ = trace "Should not happen" $ []
removeChildTwo ((values, (child,meta)):b) x = (x:values, ((child-1),meta)):b

metaSum :: ChildValues -> MetaValues -> Int
metaSum childs metas = foldl' (\acc x -> acc + valueOrZero childs x) 0 metas

valueOrZero :: [Int] -> Int -> Int
valueOrZero x index = if index > length x then 0 else x !! (index - 1)
