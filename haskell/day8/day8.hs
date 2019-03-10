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
    putStrLn $ show inputInt
    putStrLn $ show $ sumMeta inputInt

type ChildsAndMeta = (Int, Int)

sumMeta :: [Int] -> Int
sumMeta a = sumMeta' a [] 0


sumMeta' :: [Int] -> [ChildsAndMeta] -> Int -> Int
sumMeta' [] [] c = c
sumMeta' a ((0,meta):b) c =trace (show c) $ trace (show meta) $ trace (show (removeChild b)) $ sumMeta' (drop meta a) (removeChild b) strictSum
    where !strictSum = (sum $ take meta a) + c
sumMeta' (childs:meta:a) b c = if childs > 0
                                  then trace (show (childs,meta)) $ sumMeta' a ((childs,meta):b) c
                                  else trace (show (childs,meta)) $ sumMeta' (drop meta a) (removeChild b) strictSum
    where !strictSum = (sum $ take meta a) + c

removeChild :: [ChildsAndMeta] -> [ChildsAndMeta]
removeChild [] = []
removeChild ((child,meta):as) = ((child-1),meta):as
