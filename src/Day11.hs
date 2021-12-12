module Day11 where

import Lib
import Data.List

splitString :: String -> [String]
splitString line = [[str] | str <- line]

readLine :: String -> [Integer]
readLine line = map readInt (splitString line)

toLists :: [Integer] -> [[Integer]]
toLists xs = [[x] | x <- xs]

topNeighbours :: [[[Integer]]] -> [[[Integer]]]
topNeighbours points = [[] | _ <- [1..width]] : take (height - 1) points
    where
        width = length $ head points
        height = length points

bottomNeighbours :: [[[Integer]]] -> [[[Integer]]]
bottomNeighbours points = tail points ++ [[[] | _ <- [1..width]]]
    where
        width = length $ head points

rightNeighbours :: [[[Integer]]] -> [[[Integer]]]
rightNeighbours = map ((++[[]]) . tail)

leftNeighbours :: [[[Integer]]] -> [[[Integer]]]
leftNeighbours points = map (([] :) . take (width - 1)) points
    where
        width = length $ head points

topRightNeighbours :: [[[Integer]]] -> [[[Integer]]]
topRightNeighbours = topNeighbours . rightNeighbours

topLeftNeighbours :: [[[Integer]]] -> [[[Integer]]]
topLeftNeighbours = topNeighbours . leftNeighbours

bottomLeftNeighbours :: [[[Integer]]] -> [[[Integer]]]
bottomLeftNeighbours = bottomNeighbours . leftNeighbours

bottomRightNeighbours :: [[[Integer]]] -> [[[Integer]]]
bottomRightNeighbours = bottomNeighbours . rightNeighbours

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
        listPoints = map toLists points
        neighbours = [topNeighbours listPoints, bottomNeighbours listPoints,
                      rightNeighbours listPoints, leftNeighbours listPoints,
                      bottomRightNeighbours listPoints, topRightNeighbours listPoints,
                      bottomLeftNeighbours listPoints, topLeftNeighbours listPoints]

threshold :: [[Integer]] -> Bool
threshold octos = any (>9) (concat octos)

extract :: [[(Integer, [Integer])]] -> [[Integer]]
extract = map (map fst)

energize :: (Integer, [Integer]) -> Integer
energize (octo,neighbours) | octo <= 9 = octo + toInteger (length (filter (>9) neighbours))
                           | otherwise = -10

flash :: [[Integer]] -> [[Integer]]
flash octos = [map energize line | line <- groups]
    where
        groups = groupNeighbours octos

recover :: Integer -> Integer
recover octo | octo < 0 = 0
             | otherwise = octo

simulateFlash :: [[Integer]] -> ([[Integer]], Integer)
simulateFlash octos | threshold octos = simulateFlash $ flash octos
                    | otherwise = ([map recover octo | octo <- octos], toInteger $ length (filter (< 0) (concat octos)))

simulateStep :: [[Integer]] -> [[Integer]]
simulateStep = map (map (+1))

simulate' :: Integer -> Integer -> [[Integer]] -> ([[Integer]], Integer)
simulate' 0 count octos = (octos, count)
simulate' n count octos = simulate' (n-1) (count + nextCount) next
    where
        (next, nextCount) = simulateFlash $ simulateStep octos

simulate :: Integer -> Integer -> [[Integer]] -> ([[Integer]], Integer)
simulate n count octos | nextCount < 100 = simulate (n+1) (count + nextCount) next
                       | otherwise = (octos, n)
    where
        (next, nextCount) = simulateFlash $ simulateStep octos

readInput :: String -> [[Integer]]
readInput input = map readLine (lines input)

writeOutput :: Integer -> String
writeOutput = show

solve' :: [[Integer]] -> Integer
solve' input = snd $ simulate' 100 0 input

solve :: [[Integer]] -> Integer
solve input = snd $ simulate 1 0 input

_solve :: IO ()
_solve = do
    putStrLn $ exercise 11 "Dumbo Octopus"

    contents <- readFile "data/data_day11.txt"
    let input = readInput contents

    -- putStrLn $ "Number of octopus flashes: " ++ writeOutput (solve' input)

    putStrLn $ "Steps for synchronized flashes: " ++ writeOutput (solve input)


_main :: IO ()
_main = interact (writeOutput . solve . readInput)