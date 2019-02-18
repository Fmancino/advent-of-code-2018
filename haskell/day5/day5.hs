import Data.Char

main = do
    input <- getContents
    putStrLn $ show $ length (traverseRmDuplicate input) - 1
    putStrLn $ show $ (findShortest input) - 1


invertCase :: Char -> Char
invertCase a
    | (a >= 'a') && (a <= 'z') = toUpper a
    | otherwise = toLower a

isSameInverted :: (Char, Char) -> Bool
isSameInverted (a, b) = (invertCase a) == b

traverseRmDuplicate :: [Char] -> [Char]
traverseRmDuplicate i = traverseRmDuplicateImpl [] (head i, head $ tail i) (tail $ tail i)

traverseRmDuplicateImpl :: [Char] -> (Char, Char) -> [Char] -> [Char]
traverseRmDuplicateImpl before comp [] =
          if isSameInverted comp
              then reverse before
              else (reverse before) ++ [fst comp, snd comp]
traverseRmDuplicateImpl [] comp after =
          if isSameInverted comp
              then traverseRmDuplicateImpl [] (head after, head $ tail after) (tail $ tail after)
              else traverseRmDuplicateImpl [fst comp] (snd comp, head after) (tail after)
traverseRmDuplicateImpl before comp after =
          if isSameInverted comp
              then traverseRmDuplicateImpl (tail before) (head before, head after) (tail after)
              else traverseRmDuplicateImpl ([fst comp] ++ before ) (snd comp, head after) (tail after)


findShortest :: [Char] -> Int
findShortest i = minimum $ map (length . traverseRmDuplicate) [filter (\x -> x /= a && x /= toUpper a) i | a <- ['a'..'z']]
