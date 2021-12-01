module Day1 where


import Data.List ()
import Helper (exercise)
import System.IO ()
import Control.Monad ()

readInt :: String -> Int
readInt = read

sucIncreasing :: [Int] -> Int
sucIncreasing (a:b:ns) | b > a = 1 + sucIncreasing (b : ns)
                       | otherwise = sucIncreasing (b : ns)
sucIncreasing [_] = 0
sucIncreasing [] = 0

poolValues :: [Int] -> Int -> [Int]
poolValues [] n = []
poolValues ns n = sum (take n ns) : poolValues (tail ns) n


_main :: IO ()
_main = do
    putStrLn $ exercise 1 "Find number of increasing depths"
    contents <- readFile "data.txt"

    let values = map readInt . words $ contents
    print $ "Successive increases in depth: " ++ show (sucIncreasing values)

    let values = poolValues (map readInt . words $ contents) 3
    print $ "Successive increases in combined measurements: " ++ show (sucIncreasing values)

