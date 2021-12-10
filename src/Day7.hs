module Day7 where

import Lib
import Data.List

calcDist :: Integer -> Integer -> Integer
calcDist a b = abs (a - b)

calcCost :: Integer -> [Integer] -> Integer
calcCost tar locs = sum ([sumSequence (calcDist tar loc) | loc <- locs])

sumSequence :: Integer -> Integer
sumSequence a = sum [1..a]

findOptimum :: Integer -> [Integer] -> Integer
findOptimum tar locs | curCost == minimum [left, curCost, right] = curCost
                     | left > right = findOptimum (tar+1) locs
                     | otherwise = findOptimum (tar-1) locs
    where
        curCost = calcCost tar locs
        right = calcCost (tar+1) locs
        left  = calcCost (tar-1) locs

calcCost' :: Integer -> [Integer] -> Integer
calcCost' tar locs = sum  [calcDist tar loc | loc <- locs]

readInput :: String -> [Integer]
readInput input = map readInt (wordsWhen (==',') input)

writeOutput :: Integer -> String
writeOutput = show

solve' :: [Integer] -> Integer
solve' locs = calcCost (locs !! (length locs `div` 2)) locs

solve :: [Integer] -> Integer
solve locs = findOptimum (toInteger ((head locs + last locs) `div` 2)) locs

_solve :: IO ()
_solve = do
    putStrLn $ exercise 7 "The Treachery of Whales"

    contents <- readFile "data/data_day7.txt"
    let locs = sort $ readInput contents
    -- let heuristic = toInteger ((head locs + last locs) `div` 2) -- heuristic start for part One
    let heuristic = sum locs `div` toInteger (length locs) -- heuristic start for part Two
    let cost = findOptimum heuristic locs
    putStrLn $ "Best estimated crab sub position: " ++ show heuristic
    putStrLn $ "Cost of crab sub movements: " ++ show (cost)

    -- let cost = calcCost (locs !! (length locs `div` 2)) locs
    -- putStrLn $ "Best crab sub position: " ++ show (locs !! (length locs `div` 2))
    -- putStrLn $ "Cost of crab sub movements: " ++ show (cost)


-- _main :: IO ()
-- _main = interact (writeOutput . solve . readInput)