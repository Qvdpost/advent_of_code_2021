module Main where

import qualified System.Environment as Env

import Lib
import Day1 (_solve, solve, readInput, writeOutput)
import Day2 (_solve, solve, readInput, writeOutput)
import Day3 (_solve, solve, readInput, writeOutput)

-- main :: IO ()
-- main = do
    -- Day1._main
    -- Day2._main
    -- Day3._main
    -- putStrLn "\n\n✨ All Done! ✨"

-- writeOutput = unlines . (map show)

main :: IO ()
main = interact (Day1.writeOutput . Day1.solve . Day1.readInput)
-- main = interact (Day2.writeOutput . Day2.solve . Day2.readInput)
-- main = interact (Day3.writeOutput . Day3.solve . Day3.readInput)

