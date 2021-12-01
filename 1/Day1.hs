{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Day1 where


import Data.List
import Helper (exercise)
import System.IO
import Control.Monad

readInt :: String -> Int
readInt = read

sucIncreasing :: [Int] -> Int
sucIncreasing (a:b:ns) | b > a = 1 + sucIncreasing (b : ns)
                       | otherwise = sucIncreasing (b : ns)
sucIncreasing [a, b] | b > a = 1
                     | otherwise = 0
sucIncreasing [_] = 0
sucIncreasing [] = 0

poolValues :: [Int] -> [Int]
poolValues (a:b:c:ns) = (a + b + c) : poolValues (b:c:ns)
poolValues [a, b, c] = [a + b + c]
poolValues [a, b] = [a + b]
poolValues [a] = [a]
poolValues [] = []

_main :: IO ()
_main = do
    putStrLn $ exercise 1 "Find number of increasing depths"
    contents <- readFile "data.txt"
    let values = poolValues $ map readInt . words $ contents

    print $ sucIncreasing values

