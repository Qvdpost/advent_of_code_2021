module Day8 where

import Lib
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

readLine :: [String] -> ([Set Char], [Set Char])
readLine line = (map Set.fromList (takeWhile (/= "|") line), map Set.fromList (tail (dropWhile (/="|") line)))

readInput :: String -> [([Set Char], [Set Char])]
readInput input = map readLine (map words (lines input))

genSigMap :: Map String (Set [Char])
genSigMap = Map.fromList [("a", Set.empty), ("b", Set.empty), ("c", Set.empty), ("d", Set.empty), ("e", Set.empty), ("f", Set.empty), ("g", Set.empty)]

genValMap :: [(String, Set Char)] -> Map String (Set Char)
genValMap values = Map.fromList (filter (\x -> fst x /= "unknown") values)

flipMap :: Ord b => Map a b -> Map b a
flipMap mapping = Map.fromList [(value, key) | (key, value) <- Map.toList mapping]

decodeSignal :: Set Char -> String
decodeSignal signal | sigLen == 2 = "1"
                    | sigLen == 3 = "7"
                    | sigLen == 4 = "4"
                    | sigLen == 7 = "8"
                    | otherwise = "unknown"
    where
        sigLen = length signal

encode0 :: Set Char -> Map String (Set Char) -> Bool
encode0 signal sigMap = sigLen == 6 && (sigMap Map.! "1") `Set.isSubsetOf` signal && signal `Set.isSubsetOf` (sigMap Map.! "8") && not ((sigMap Map.! "4") `Set.isSubsetOf` signal)
    where
        sigLen = length signal

encode2 :: Set Char -> Map String (Set Char) -> Bool
encode2 signal sigMap = sigLen == 5 && length ((sigMap Map.! "4") `Set.intersection` signal) == 2
    where
        sigLen = length signal

encode3 :: Set Char -> Map String (Set Char) -> Bool
encode3 signal sigMap = sigLen == 5 && (sigMap Map.! "1") `Set.isSubsetOf` signal && signal `Set.isSubsetOf` (sigMap Map.! "8")
    where
        sigLen = length signal

encode5 :: Set Char -> Map String (Set Char) -> Bool
encode5 signal sigMap = sigLen == 5 && length ((sigMap Map.! "4") `Set.intersection` signal) == 3 && not ((sigMap Map.! "7") `Set.isSubsetOf` signal)
    where
        sigLen = length signal

encode6 :: Set Char -> Map String (Set Char) -> Bool
encode6 signal sigMap = sigLen == 6 && not ((sigMap Map.! "1") `Set.isSubsetOf` signal) && signal `Set.isSubsetOf` (sigMap Map.! "8") && not ((sigMap Map.! "4") `Set.isSubsetOf` signal)
    where
        sigLen = length signal

encode9 :: Set Char -> Map String (Set Char) -> Bool
encode9 signal sigMap = sigLen == 6 && sigMap Map.! "4" `Set.isSubsetOf` signal
    where
        sigLen = length signal

decodeSignals :: [Set Char] -> Map String (Set Char) -> Map String (Set Char)
decodeSignals [] valMap = valMap
decodeSignals (signal:signals) valMap | encode0 signal valMap = Map.insert "0" signal (decodeSignals signals valMap)
                                      | encode2 signal valMap = Map.insert "2" signal (decodeSignals signals valMap)
                                      | encode3 signal valMap = Map.insert "3" signal (decodeSignals signals valMap)
                                      | encode5 signal valMap = Map.insert "5" signal (decodeSignals signals valMap)
                                      | encode6 signal valMap = Map.insert "6" signal (decodeSignals signals valMap)
                                      | encode9 signal valMap = Map.insert "9" signal (decodeSignals signals valMap)
                                      | otherwise = decodeSignals signals valMap

decodeOutput :: [Set Char] -> Map String (Set Char) -> Integer
decodeOutput output valMap = read $ concat [sigMap Map.! value | value <- output]
    where
        sigMap = flipMap valMap

decodeEntry :: ([Set Char], [Set Char]) -> Integer
decodeEntry (signals, output) =  decodeOutput output valMap
    where
        signal = head signals
        valMap = decodeSignals signals (genValMap (zip (map decodeSignal signals) signals))

writeOutput :: Integer -> String
writeOutput = show

solve' :: [Integer] -> Integer
solve' locs = 0

solve :: [([Set Char], [Set Char])] -> Integer
solve input = sum (map decodeEntry input)

_solve :: IO ()
_solve = do
    putStrLn $ exercise 8 "Seven Segment Search"

    contents <- readFile "data/data_day8.txt"
    let entries = readInput contents
    let values = map decodeEntry entries
    putStrLn $ "Seven segment decoded sum: " ++ show (sum values)


-- _main :: IO ()
-- _main = interact (writeOutput . solve . readInput)