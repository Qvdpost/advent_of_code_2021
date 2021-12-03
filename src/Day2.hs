module Day2 where

import Lib (exercise)

readInt :: String -> Integer
readInt = read

readMoves :: [String] -> [(String, Integer)]
readMoves (dir:dist:moves) = (dir, readInt dist) : readMoves moves
readMoves [dir] = [(dir, 0)]
readMoves [] = []

calcLoc :: [(String, Integer)] -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
calcLoc ((dir, dist):moves) (x, y, z) | dir == "forward" = calcLoc moves (x + dist, y + (z * dist), z)
                                      | dir == "up" = calcLoc moves (x, y, z - dist)
                                      | dir == "down" = calcLoc moves (x, y, z + dist)
                                      | otherwise = calcLoc moves (x, y, z)
calcLoc [] (x, y, z) = (x, y, z)

mulLoc :: (Integer, Integer, Integer) -> Integer
mulLoc (a, b, c) = a * b

readInput :: String -> [(String, Integer)]
readInput = readMoves . words

writeOutput :: Integer -> String
writeOutput = show

solve :: [(String, Integer)] -> Integer
solve moves = mulLoc $ calcLoc moves (0,0,0)

_solve :: IO ()
_solve = do
    putStrLn $ exercise 2 "Calculate submarine destination of a sequence of moves"

    contents <- readFile "data/data_day2.txt"
    let moves = readMoves $ words contents
    let loc = calcLoc moves (0,0,0)

    putStrLn $ "Location and direction after moves: " ++ show loc

    putStrLn $ "Multiplied location: " ++ show (mulLoc loc)

_main :: IO ()
_main = interact (writeOutput . solve . readInput)