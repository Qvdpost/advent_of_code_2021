module Day3 where

import Lib

readBit :: Char -> Integer
readBit c | c == '0' = -1
          | c == '1' = 1
          | otherwise = 0

readBits :: String -> [Integer]
readBits = map readBit

countBits :: [[Integer]] -> [Integer]
countBits (a:b:rest) = countBits (zipWith (+) a b:rest)
countBits [a] = a
countBits [] = []

mostCommonBits :: [Integer] -> [Integer]
mostCommonBits (bit:bits) | bit > 0 = 1 : mostCommonBits bits
                          | bit < 0 = 0 : mostCommonBits bits
                          | otherwise = 1 : mostCommonBits bits
mostCommonBits [] = []

leastCommonBits :: [Integer] -> [Integer]
leastCommonBits (bit:bits) | bit < 0 = 1 : leastCommonBits bits
                           | bit > 0 = 0 : leastCommonBits bits
                           | otherwise = 0 : leastCommonBits bits
leastCommonBits [] = []

bitsToDec :: [Integer] -> Integer
bitsToDec (bit:bits) = bit * (2^length bits) + bitsToDec bits
bitsToDec [] = 0

getBit :: Integer -> Integer
getBit b | b == -1 = 0
         | otherwise = b

filterBits :: [[Integer]] -> ([Integer] -> [Integer]) -> [Integer]
filterBits [a] func = map getBit a
filterBits [] func = []
filterBits bits func = fBit : filterBits remainder func
    where
        fBit = head $ func $ countBits $ bits
        filtered = filter (\x -> fBit == getBit (head x)) bits
        remainder = map (drop 1) filtered

readInput :: String -> [[Integer]]
readInput = map readBits . words

writeOutput :: Integer -> String
writeOutput = show

solve' :: [[Integer]] -> Integer
solve' binary = gamma * epsilon
    where
        gamma = bitsToDec (mostCommonBits $ countBits binary)
        epsilon = bitsToDec (leastCommonBits $ countBits binary)

solve :: [[Integer]] -> Integer
solve binary = oxy * scrub
    where
        oxy = bitsToDec (filterBits binary mostCommonBits)
        scrub = bitsToDec (filterBits binary leastCommonBits)

_solve :: IO ()
_solve = do
    putStrLn $ exercise 3 "Calculate submarine power consumption"

    contents <- readFile "data/data_day3.txt"
    let binary = map readBits $ words contents

    let gamma = bitsToDec (mostCommonBits $ countBits binary)
    let oxy = bitsToDec (filterBits binary mostCommonBits)
    putStrLn $ "Gamma rate: " ++ show gamma
    putStrLn $ "Oxy Gen rate: " ++ show oxy

    let epsilon = bitsToDec (leastCommonBits $ countBits binary)
    let scrub = bitsToDec (filterBits binary leastCommonBits)
    putStrLn $ "\nEpsilon rate: " ++ show epsilon
    putStrLn $ "CO2 scrubber rate: " ++ show scrub

    putStrLn $ "\nPower consumption: " ++ show (gamma * epsilon)
    putStrLn $ "Life support rating: " ++ show (oxy * scrub)

_main :: IO ()
_main = interact (writeOutput . solve . readInput)