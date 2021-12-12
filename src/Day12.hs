module Day12 where

import Lib
import Debug.Trace
import Data.Char ( isUpper )
import Data.Map (Map)
import qualified Data.Map as Map

type Path = ([String], Bool)

insertPair :: String -> String -> Map String [String] -> Map String [String]
insertPair key value mapping | Map.member key mapping = Map.insert key (value : (mapping Map.! key)) mapping
                             | otherwise = Map.insert key [value] mapping

addLine :: String -> Map String [String] -> Map String [String]
addLine line mapping = insertPair (last pair) (head pair) (insertPair (head pair) (last pair) mapping)
    where
        pair = wordsWhen (=='-') line

addLines :: [String] -> Map String [String] -> Map String [String]
addLines lines mapping = foldl (flip addLine) mapping lines

isBig :: String -> Bool
isBig = all isUpper

count :: String -> [String] -> Int
count a path = length (filter (==a) path)

smallCaveValidation :: String -> [String] -> Bool
smallCaveValidation a path = a `notElem` path

validation :: String -> Path -> Map String [String] -> Bool
validation a path graph | isBig a = a `notElem` illegals && count a (pathWay path) <= length (graph Map.! a)
                        | otherwise = a `notElem` illegals && (not (doubledPath path) || a `notElem` pathWay path)
    where
        illegals = ["start"]

pathWays :: Path -> Map String [String] -> [Path]
pathWays path graph = [(pathWay path++[option], checkDoubled option path) | option <- connections]
    where
        connections = filter (\x -> validation x path graph) (graph Map.! last (pathWay path))

pathWay :: Path -> [String]
pathWay (path, _) = path

doubledPath :: Path -> Bool
doubledPath (_, doubled) = doubled

checkDoubled :: String -> Path -> Bool
checkDoubled a (path,doubled) | doubled = True
                              | not (isBig a) = a `elem` path
                              | otherwise = False

bfs :: [Path] -> Map String [String] -> [Path]
bfs [] graph = []
bfs (path:paths) graph| last (pathWay path) == "end" = path : bfs paths graph
                      | otherwise =  bfs (paths++newPaths) graph
    where
        newPaths = pathWays path graph

dfs :: [Path] -> Map String [String] -> [Path]
dfs [] graph = []
dfs (path:paths) graph| last (pathWay path) == "end" = path : dfs paths graph
                      | otherwise =  dfs (newPaths++paths) graph
    where
        newPaths = pathWays path graph

readInput :: String -> Map String [String]
readInput input = addLines (lines input) mapping
    where
        mapping = Map.empty

writeOutput :: Integer -> String
writeOutput = show

solve :: Map String [String] -> Integer
solve input = toInteger $ length $ dfs [(["start"], False)] input

_solve :: IO ()
_solve = do
    putStrLn $ exercise 12 "Passage Pathing"

    contents <- readFile "data/data_test.txt"

    let graph = readInput contents

    putStrLn $ "Number of paths: " ++ writeOutput (solve graph)

_main :: IO ()
_main = interact (writeOutput . solve . readInput)