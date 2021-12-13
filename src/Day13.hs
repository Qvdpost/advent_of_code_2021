module Day13 where

import Lib
import Data.List ( transpose )

readCoord :: String -> (Integer, Integer)
readCoord coord = (readInt (head coords), readInt (last coords))
    where
        coords = wordsWhen (==',') coord

readFold :: String -> (String, Int)
readFold line = (head instr, read (last instr))
    where
        instr = wordsWhen (=='=') (last (words line))

maxX :: [(Integer, Integer)] -> Integer
maxX coords = maximum [x | (x,y) <- coords]

maxY :: [(Integer, Integer)] -> Integer
maxY coords = maximum [y | (x,y) <- coords]

dotColumns :: Integer -> [(Integer, Integer)] -> [Integer]
dotColumns row coords = [x | (x,y) <- coords, row == y]

insertDot :: Integer -> [Char] -> [Char]
insertDot _ [] = []
insertDot 0 (c:row) = '#' : row
insertDot column (c:row) = c : insertDot (column - 1) row

insertDots :: [Integer] -> [Char] -> [Char]
insertDots columns row = foldl (flip insertDot) row columns

mergeDot :: (Char, Char) -> Char
mergeDot (a,b) | a == '#' || b == '#' = '#'
               | otherwise = '.'

mergeDots :: [Char] -> [Char] -> [Char]
mergeDots = zipWith (curry mergeDot)

foldMapAt :: (String, Int) -> [[Char]] -> ([[Char]], [[Char]])
foldMapAt (orientation, foldLine) dotMap | orientation == "y" = let (a, b) = splitAt foldLine dotMap in (a, reverse b)
                                         | otherwise = let (a, b) = splitAt foldLine (transpose dotMap) in (transpose a, map reverse (transpose b))

mergeFolds :: (String, Int) -> [[Char]] -> [[Char]]
mergeFolds foldLine dotMap = zipWith mergeDots left right
    where
        (left,right) = foldMapAt foldLine dotMap

printMap :: [[Char]] -> IO ()
printMap [] = print ""
printMap (row:dotMap) = do
    print row
    printMap dotMap

countDots :: [[Char]] -> Integer
countDots = foldl (\x y -> x + toInteger (length (filter (=='#') y))) 0

readInput :: String -> ([[Char]], [(String, Int)])
readInput input = ([insertDots (dotColumns row coords) ['.' | _ <- [0..maxX coords]] | row <- [0..maxY coords]], map readFold (tail (dropWhile (/="") (lines input))))
    where
        coords = map readCoord (takeWhile (/="") (lines input))

writeOutput :: [[Char]] -> String
writeOutput = concatMap (++['\n'])

solve :: ([[Char]], [(String, Int)]) -> [[Char]]
solve (dotMap, []) = dotMap
solve (dotMap, (instr:instrs)) = solve (mergeFolds instr dotMap, instrs)

_solve :: IO ()
_solve = do
    putStrLn $ exercise 13 "Transparent Origami"

    contents <- readFile "data/data_day13.txt"
    let input = readInput contents
    let step1 = mergeFolds (head $ snd input) (fst input)

    let value = solve $ readInput contents

    putStrLn $ "Number of dots: " ++ show (countDots value)

    putStrLn $ "Activation Code: \n" ++ writeOutput value

_main :: IO ()
_main = interact (writeOutput . solve . readInput)