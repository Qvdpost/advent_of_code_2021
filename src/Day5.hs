module Day5 where

import Lib
import Data.List

readInt :: String -> Integer
readInt = read

readCoord :: String -> (Integer, Integer)
readCoord line = (head coords, last coords)
    where
        coords = map readInt (wordsWhen (==',') line)

readLine :: [String] -> ((Integer, Integer), (Integer, Integer))
readLine line = (readCoord (head line), readCoord (last line))

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

getSequence :: Integer -> Integer -> [Integer]
getSequence a b | a > b = reverse [b..a]
                | otherwise = [a..b]

drawVentLine :: ((Integer, Integer), (Integer, Integer)) -> [(Integer, Integer)]
drawVentLine ((x1,y1),(x2,y2)) | x1 == x2 = [(x1, y) | y <- [min y1 y2.. max y1 y2]]
                               | y1 == y2 = [(x, y1) | x <- [min x1 x2.. max x1 x2]]
                               | otherwise = zip (getSequence x1 x2) (getSequence y1 y2)

countIntensity :: [(Integer, Integer)] -> Int
countIntensity [] = 0
countIntensity coords = length $ filter (\x -> snd x > 1) intensities
    where
        intensities = map (\xs@(x:_) -> (x, length xs)) . group . sort $ coords

readInput :: String -> [((Integer, Integer), (Integer, Integer))]
readInput input = map readLine (map words (lines input))

writeOutput :: Integer -> String
writeOutput = show

solve' :: [[Integer]] -> Integer
solve' binary = 0

solve :: [((Integer, Integer), (Integer, Integer))] -> Int
solve coords = countIntensity $ concat $ map drawVentLine coords

_solve :: IO ()
_solve = do
    putStrLn $ exercise 5 "Hydrothermal Venture"

    contents <- readFile "data/data_day5.txt"
    let coords = readInput contents
    let ventLines = map drawVentLine coords

    putStrLn $ "Number of overlapping vents: " ++ show (countIntensity $ concat ventLines)


-- _main :: IO ()
-- _main = interact (writeOutput . solve . readInput)