module Day10 where

import Lib
import Data.List

readInput :: String -> [String]
readInput = lines

matchOpen :: Char -> Bool
matchOpen c = c `elem` ['(','[','{','<']

matchClosing :: Char -> Bool
matchClosing c = c `elem` [')',']','}','>']

bracketMap :: Char -> Char
bracketMap c | c == '(' = ')'
             | c == '{' = '}'
             | c == '[' = ']'
             | c == '<' = '>'
             | c == ')' = '('
             | c == '}' = '{'
             | c == ']' = '['
             | c == '>' = '<'
             | otherwise = '|'

findClosing :: Char -> String -> String
findClosing clos [] = ""
findClosing clos (c:line) | matchOpen c = findClosing clos (parseLine (c:line))
                          | c == '-' = c : take 1 line
                          | clos /= c = '-' : [c]
                          | otherwise = line

parseLine :: String -> String
parseLine [] = ""
parseLine (c:line) | matchClosing c = c : parseLine line
                   | c == '-' = c : take 1 line
                   | otherwise = findClosing (bracketMap c) line


consumeClosure :: Char -> String -> String
consumeClosure clos [] = ""
consumeClosure clos (c:line) | matchOpen c = consumeClosure clos (parseLine (c:line))
                             | otherwise = line

fixLine :: String -> String
fixLine [] = ""
fixLine (c:line) | matchClosing c = c : fixLine line
                 | otherwise = fixLine (consumeClosure (bracketMap c) line)

score :: String -> Integer
score line | line == "-)" = 3
           | line == "-]" = 57
           | line == "-}" = 1197
           | line == "->" = 25137
           | otherwise = 0

fixScore :: String -> Integer
fixScore line | line == ")" = 1
              | line == "]" = 2
              | line == "}" = 3
              | line == ">" = 4
              | otherwise = 0

calcFix :: Integer -> String -> Integer
calcFix x [] = x
calcFix base (c:line) = calcFix (5 * base + fixScore [c]) line

calcFixScore :: [String] -> Integer
calcFixScore [] = 0
calcFixScore lines = sort (map (calcFix 0) lines) !! mid
    where
        mid = length lines `div` 2

calcScore :: [String] -> Integer
calcScore [] = 0
calcScore (line:lines) | length line == 2 = score line + calcScore lines
                       | otherwise  = calcScore lines

mirrorLine :: String -> String
mirrorLine line = reverse $ map bracketMap line

filterCorrupt :: [String] -> [String] -> [String]
filterCorrupt lines result = [line | (line, fix) <- filter (\(x,y) -> null y || head y /= '-') (zip lines result)]

writeOutput :: Integer -> String
writeOutput = show

solve' :: [Integer] -> Integer
solve' locs = 0

solve :: [[Integer]] -> Integer
solve input = 0

_solve :: IO ()
_solve = do
    putStrLn $ exercise 10 "Syntax Scoring"

    contents <- readFile "data/data_day10.txt"
    let result = map parseLine (readInput contents)
    print $ calcScore result

    let incomplete = filterCorrupt (readInput contents) result
    let fixes = map (fixLine . mirrorLine) incomplete

    putStrLn $ "Syntax fix score: " ++ show (calcFixScore fixes)

-- _main :: IO ()
-- _main = interact (writeOutput . solve . readInput)