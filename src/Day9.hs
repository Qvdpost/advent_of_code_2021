module Day9 where

import Lib

splitString :: String -> [String]
splitString line = [[str] | str <- line]

readLine :: String -> [Integer]
readLine line = map readInt (splitString line)

readInput :: String -> [[Integer]]
readInput input = map readLine (lines input)

toLists :: [Integer] -> [[Integer]]
toLists xs = [[x] | x <- xs]

topNeighbours :: [[Integer]] -> [[[Integer]]]
topNeighbours points = [[] | _ <- [1..width]] : map toLists (take (height - 1) points)
    where
        width = length $ head points
        height = length points

bottomNeighbours :: [[Integer]] -> [[[Integer]]]
bottomNeighbours points = map toLists (tail points) ++ [[[] | _ <- [1..width]]]
    where
        width = length $ head points

rightNeighbours :: [[Integer]] -> [[[Integer]]]
rightNeighbours = map (((++[[]]) . toLists) . tail)

leftNeighbours :: [[Integer]] -> [[[Integer]]]
leftNeighbours points = map ((([] :) . toLists) . take (width - 1)) points
    where
        width = length $ head points

joinNeighbours :: ([[Integer]], [[Integer]]) -> [[Integer]]
joinNeighbours (a,b) = zipWith (++) a b

merge :: [[[Integer]]] -> [[[Integer]]] -> [[[Integer]]]
merge = zipWith (curry joinNeighbours)

mergeNeighbours :: [[[[Integer]]]] -> [[[Integer]]]
mergeNeighbours [] = []
mergeNeighbours [a] = a
mergeNeighbours (a:b:rest) = mergeNeighbours (merge a b:rest)

groupNeighbours :: [[Integer]] -> [[(Integer, [Integer])]]
groupNeighbours points = zipWith zip points (mergeNeighbours neighbours)
    where
        neighbours = [topNeighbours points, bottomNeighbours points, rightNeighbours points, leftNeighbours points]

calcRisk :: [(Integer, [Integer])] -> Integer
calcRisk [] = 0
calcRisk ((val, neighbours):rest) | val < minimum neighbours = 1 + val + calcRisk rest
                                  | otherwise = calcRisk rest

writeOutput :: Integer -> String
writeOutput = show

solve' :: [Integer] -> Integer
solve' locs = 0

solve :: [[Integer]] -> Integer
solve input = 0

_solve :: IO ()
_solve = do
    putStrLn $ exercise 9 "Smoke Basin"

    contents <- readFile "data/data_day9.txt"
    let points = readInput contents
    let groups = groupNeighbours points
    putStrLn $ "Smoke basin risk level: " ++ show (calcRisk $ concat groups)


-- _main :: IO ()
-- _main = interact (writeOutput . solve . readInput)