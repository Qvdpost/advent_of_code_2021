module Day4 where

import Lib

isSubset :: [Integer] -> [Integer] -> Bool
isSubset [] _ = False
isSubset _ [] = False
isSubset a b = all (`elem` b) a

checkBingo :: [Integer] -> [[Integer]] -> Bool
checkBingo ns = any (`isSubset` ns)

playBingoRound :: [Integer] -> [[[Integer]]] -> [[[Integer]]]
playBingoRound ns [] = []
playBingoRound ns (card:cards) | checkBingo ns card = card : playBingoRound ns cards
                               | otherwise = playBingoRound ns cards

orderedBingoWinners :: Int -> [Integer] -> [[[Integer]]] -> [([Integer], [[Integer]])]
orderedBingoWinners n ns cards | n >= length ns = []
                               | null results = orderedBingoWinners (n+1) ns cards
                               | otherwise = [(input, result) | result <- results] ++ orderedBingoWinners (n+1) ns (removeCards results cards)
    where
        input = take n ns
        results = playBingoRound input cards


readInput :: String -> ([Integer], [[[Integer]]])
readInput input = (ns, cards)
    where
        content = lines input
        ns = map readInt (wordsWhen (==',') (head content))
        cards = createCards (dropWhile (=="") (tail content))

consumeLines :: [String] -> [[Integer]]
consumeLines lines = map (map readInt . words) (takeWhile (/= "") lines)

transposeLines :: [[Integer]] -> [[Integer]]
transposeLines ([]:rest) = []
transposeLines lines = concatMap (take 1) lines : transposeLines [drop 1 line | line <- lines]

createCards :: [String] -> [[[Integer]]]
createCards [] = []
createCards lines = (cardLines ++ transposeLines cardLines) : createCards (drop (length cardLines + 1) lines)
    where
        cardLines = consumeLines lines

removeCards :: [[[Integer]]] -> [[[Integer]]] -> [[[Integer]]]
removeCards _ []                 = []
removeCards xs (y:ys) | y `elem` xs   = removeCards xs ys
                      | otherwise = y : removeCards xs ys

remainingNumbers :: [Integer] -> [[Integer]] -> [Integer]
remainingNumbers ns card = concatMap (filter (`notElem` ns)) (take (length card `div` 2) card)

writeOutput :: Integer -> String
writeOutput = show

solve' :: [[Integer]] -> Integer
solve' binary = 0

solve :: [[Integer]] -> Integer
solve binary = 0

_solve :: IO ()
_solve = do
    putStrLn $ exercise 4 "Play Bingo with a giant squid"

    contents <- readFile "data/data_day4.txt"
    let (input, cards) = readInput contents
    let winners = orderedBingoWinners 1 input cards
    let (winput, winner) = head winners

    putStrLn $ "Winning multiplication: " ++ show (last winput * sum (remainingNumbers winput winner))

    let (linput, loser) = last winners
    putStrLn $ "Losing multiplication: " ++ show (last linput * sum (remainingNumbers linput loser))

-- _main :: IO ()
-- _main = interact (writeOutput . solve . readInput)