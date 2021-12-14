module Day14 where

import Lib
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort, group)
import qualified Debug.Trace as Debug

genInsertMap :: [String] -> Map String (String,String)
genInsertMap input = Map.fromList [(key, ("", head key : value)) | (key, value) <- mapInput]
    where
        mapInput = map ((\x -> (head x, last x)) . words) input

genTransformMap :: Integer -> Map String (String, String) -> Map String ((String, String),String)
genTransformMap n insertMap = Map.mapWithKey (\key (conv, value) -> (converge key value insertMap, value)) insertMap

isConverged :: String ->  Bool
isConverged a | take 3 a == take 3 (drop 3 a) = True
              | otherwise = False

converge :: String -> String -> Map String (String, String) -> (String, String)
converge key value insertMap | isConverged transformation = (take 3 transformation, drop 6 transformation)
                             | otherwise = ("", value)
    where
        transformation = simulate 3 value insertMap

transform :: String -> Map String (String,String) -> String
transform a transformMap = value
    where
        (conv, value) = transformMap Map.! a

polymerize :: String -> Map String (String,String) -> String
polymerize [] _ = ""
polymerize [a] _ = [a]
polymerize (a:b:rest) insertMap = transform (a : [b]) insertMap ++ polymerize (b : rest) insertMap

polyPairs :: String -> [(Char, Char)]
polyPairs [] = []
polyPairs [a] = []
polyPairs polymer = (head polymer, head $ tail polymer) : polyPairs (drop 1 polymer)

simulate :: Integer -> String -> Map String (String, String) -> String
simulate 0 polymer _ = polymer
simulate n polymer insertMap = simulate (n-1) (polymerize polymer insertMap) insertMap

transform' :: (Char,Char) -> Map String (String,String) -> Char
transform' (a,b) transformMap = last value
    where
        (conv, value) = transformMap Map.! [a,b]

polymerize' :: (Char, Char) -> Map String (String, String) -> [(Char, Char)]
polymerize' (a,b) insertMap = [(a, transformation), (transformation, b)]
    where
        transformation = transform' (a,b) insertMap

dfs :: Integer -> [(Char, Char)] -> Map String (String, String) -> Map Char Int
dfs 0 pairs insertMap = countElements (polyFromList pairs)
dfs n [] insertMap = Map.empty
dfs n (pair:pairs) insertMap = Map.unionWith (+) (dfs (n-1) (polymerize' pair insertMap) insertMap) (dfs n pairs insertMap)


polyFromList ::  [(Char, Char)] -> String
polyFromList pairs = concat [[x] | (x, y) <- pairs] 

countElements :: String -> Map Char Int
countElements input = Map.fromList (map (\xs@(x:_) -> (x, length xs)) . group . sort $ input)

readInput :: String -> (String, Map String (String, String))
readInput input = (polymer, genInsertMap insertions)
    where
        inputLines = lines input
        polymer = head inputLines
        insertions = drop 2 inputLines

writeOutput :: Integer -> String
writeOutput = show

solve :: (String, Map String (String, String)) -> Integer
solve _ = 0
-- solve (polymer, insertMap) = toInteger (maximum counts - minimum counts)
--     where
--         transformMap = genTransformMap 0 insertMap
--         polymerized = simulate 10 polymer insertMap
--         counts = countElements polymerized

_solve :: IO ()
_solve = do
    putStrLn $ exercise 14 "Extended Polymerization"

    contents <- readFile "data/data_test.txt"

    let (polymer, insertMap) = readInput contents
    let counts = dfs 40 (polyPairs polymer) insertMap

    print $ counts
    print $ toInteger (maximum counts - minimum counts)

    -- putStrLn $ "PLACEHOLDER MESSAGE: " ++ writeOutput value

_main :: IO ()
_main = interact (writeOutput . solve . readInput)