module Day6 where

import Lib
import Data.List

insertGen :: (Integer, Int) -> [(Integer, Int)] -> [(Integer, Int)]
insertGen new [] = [new]
insertGen (iAge, iCount) ((age, count):others) | iAge == age = (age, count+iCount) : others
                                               | iAge < age = (iAge, iCount) : (age, count) : others
                                               | otherwise = (age, count) : insertGen (iAge, iCount) others

simulate :: Int -> [(Integer, Int)] -> [(Integer, Int)]
simulate _ [] = []
simulate 0 fishes = fishes
simulate n ((fAge,fCount):fishes) | fAge >= 1 = simulate (n-1) nextGen
                                  | otherwise = simulate (n-1) (insertGen (8, fCount) (insertGen (6, fCount) (tail nextGen)))
    where
        nextGen = [(x-1, count) | (x, count) <- (fAge,fCount):fishes]

countFish :: [(Integer, Int)] -> Int
countFish [] = 0
countFish ((_,count):fish) = count + countFish fish

readInput :: String -> [(Integer, Int)]
readInput input = map (\xs@(x:_) -> (x, length xs)) . group . sort $ map readInt (wordsWhen (==',') input)

writeOutput :: Integer -> String
writeOutput = show

solve' :: [[Integer]] -> Integer
solve' binary = 0

solve :: [((Integer, Integer), (Integer, Integer))] -> Int
solve coords = 0

_solve :: IO ()
_solve = do
    putStrLn $ exercise 6 "Lanternfish"

    contents <- readFile "data/data_day6.txt"
    let fishes = readInput contents
    putStrLn $ "Number of Lanternfish in the sea: " ++ show (countFish $ simulate 256 fishes)


-- _main :: IO ()
-- _main = interact (writeOutput . solve . readInput)