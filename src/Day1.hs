module Day1 where

import Lib (exercise)

readInt :: String -> Integer
readInt = read

sucIncreasing :: [Integer] -> Integer
sucIncreasing (a:b:ns) | b > a = 1 + sucIncreasing (b : ns)
                       | otherwise = sucIncreasing (b : ns)
sucIncreasing [_] = 0
sucIncreasing [] = 0

poolValues :: Int -> [Integer] -> [Integer]
poolValues n [] = []
poolValues n ns = sum (take n ns) : poolValues n (tail ns)

readInput :: String -> [Integer]
readInput = map readInt . words

writeOutput :: Integer -> String
writeOutput = show

-- Successive increases in measurements
solve' :: [Integer] -> Integer
solve' = sucIncreasing

-- Successive increases in combined measurements
solve :: [Integer] -> Integer
solve = sucIncreasing . poolValues 3

_solve :: IO ()
_solve = do
    putStrLn $ exercise 1 "Find number of increasing depths"
    contents <- readFile "data/data_day1.txt"

    let values = map readInt . words $ contents
    putStrLn $ "Successive increases in depth: " ++ show (sucIncreasing values)

    let values = poolValues 3 (map readInt . words $ contents)
    putStrLn $ "Successive increases in combined measurements: " ++ show (sucIncreasing values)

_main :: IO ()
_main = interact (writeOutput . solve . readInput)
