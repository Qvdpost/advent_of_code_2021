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

genTransformMap :: Integer -> Map String (String, String) -> Map String (String,String)
genTransformMap n insertMap = Map.mapWithKey (\key (conv, value) -> converge key value insertMap) insertMap

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

-- simulate' :: String -> Map String String -> String
-- simulate' [] _ = "  "
-- simulate' polymer transformMap = init (polymerize (take 2 polymer) transformMap) ++ tail (simulate' (tail polymer) transformMap)

-- simulate'' :: Integer -> String -> Map String String -> Map Char Int
-- simulate'' 0 polymer _ =  countElements polymer -- let counts = [snd x | x <- countElements polymer] in toInteger (maximum counts - minimum counts)
-- simulate'' n [a] _ = countElements [a] -- let counts = [snd x | x <- countElements [a]] in toInteger (maximum counts - minimum counts)
-- simulate'' n [] _ = Map.empty
-- simulate'' n (a:b:rest) insertMap = Map.unionWith (+) (simulate'' (n-1) (init $ polymerize (a: [b]) insertMap) insertMap) (simulate'' n (b:rest) insertMap)

-- simulate''' :: Integer -> String -> Map String String -> String
-- simulate''' 0 polymer _ =  polymer -- let counts = [snd x | x <- countElements polymer] in toInteger (maximum counts - minimum counts)
-- simulate''' n [a] _ = [a] -- let counts = [snd x | x <- countElements [a]] in toInteger (maximum counts - minimum counts)
-- simulate''' n [] _ = ""
-- simulate''' n (a:b:rest) insertMap = simulate''' (n-1) (init $ polymerize (a: [b]) insertMap) insertMap ++ (simulate''' n (b:rest) insertMap)

polymerize' :: (Char, Char) -> Map String (String,String) -> String
polymerize' (a,b) = transform (a : [b])

polyPairs :: String -> [(Char, Char)]
polyPairs [] = []
polyPairs [a] = [(a, ' ')]
polyPairs polymer = (head polymer, head $ tail polymer) : polyPairs (drop 1 polymer)

simulate' :: [(Char, Char)] -> Map String (String,String) -> String
simulate' [] insertMap = ""
simulate' [(a, ' ')] insertMap = [a]
simulate' [pair] insertMap = polymerize' pair insertMap
simulate' (pair:pairs) insertMap = polymerize' pair insertMap ++ simulate' pairs insertMap

simulate'' :: Integer -> [(Char, Char)] -> Map String (String,String) -> String
simulate'' 1 pairs insertMap = simulate' pairs insertMap
simulate'' n pairs insertMap = simulate'' (n-1) (polyPairs (simulate' pairs insertMap)) insertMap

simulate :: Integer -> String -> Map String (String, String) -> String
simulate 0 polymer _ = polymer
simulate n polymer insertMap = simulate (n-1) (polymerize polymer insertMap) insertMap

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

    let transformMap = genTransformMap 3 insertMap

    let polymerized = simulate'' 2 (polyPairs polymer) transformMap
    
    print polymerized
    -- let occurrences = countElements polymerized

    -- print $ toInteger (maximum occurrences - minimum occurrences)

    print transformMap

    -- let polymerized = simulate'' 2 polymer insertMap
    -- print polymerized
    -- print $ toInteger (maximum polymerized - minimum polymerized)

    -- print $ simulate''' 2 polymer insertMap
    -- let occurrences = countElements polymerized
    -- let counts = [snd x | x <- occurrences]

    -- let value = solve $ readInput contents

    -- putStrLn $ "PLACEHOLDER MESSAGE: " ++ writeOutput value

_main :: IO ()
_main = interact (writeOutput . solve . readInput)