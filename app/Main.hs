module Main where

import Lib
import Day1 (_main)
import Day2 (_main)

main :: IO ()
main = do
    Day1._main
    Day2._main
    putStrLn "\n\n✨ All Done! ✨"