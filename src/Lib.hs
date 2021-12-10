module Lib
    ( exercise,
      tupleToList,
      readInt,
      wordsWhen
    ) where

tupleToList :: (Integer, Integer, Integer, Integer) -> [Integer]
tupleToList (a,b,c,d) = [a,b,c,d]

readInt :: String -> Integer
readInt = read

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


exercise :: Integer -> String -> String
exercise x name
  | x == 1 = banner
  | otherwise = "\n" ++ banner
  where
    contents = " | Exercise " ++ show x ++ ": " ++ name ++ "| "
    len = length contents -4
    delimiter = "=|" ++ replicate len '-' ++ "|="
    banner = delimiter ++ "\n" ++ contents ++ "\n" ++ delimiter ++ "\n"