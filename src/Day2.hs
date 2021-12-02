module Day2 where

import Helper (exercise)

readInt :: String -> Int
readInt = read

readMoves :: [String] -> [(String, Int)]
readMoves (dir:dist:moves) = (dir, readInt dist) : readMoves moves
readMoves [dir] = [(dir, 0)]
readMoves [] = []

calcLoc :: [(String, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
calcLoc ((dir, dist):moves) (x, y, z) | dir == "forward" = calcLoc moves (x + dist, y + (z * dist), z)
                                      | dir == "up" = calcLoc moves (x, y, z - dist)
                                      | dir == "down" = calcLoc moves (x, y, z + dist)
                                      | otherwise = calcLoc moves (x, y, z)
calcLoc [] (x, y, z) = (x, y, z)

mulLoc :: (Int, Int, Int) -> Int
mulLoc (a, b, c) = a * b

_main :: IO ()
_main = do
    putStrLn $ exercise 2 "Calculate destination of a sequence of moves"

    contents <- readFile "data/data_day2.txt"
    let moves = readMoves $ words contents
    let loc = calcLoc moves (0,0,0)

    putStrLn $ "Location and direction after moves: " ++ show loc

    putStrLn $ "Multiplied location: " ++ show (mulLoc loc)

