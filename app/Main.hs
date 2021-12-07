module Main where

import qualified System.Environment as Env

import Lib
import Day1 (_solve, solve, readInput, writeOutput)
import Day2 (_solve, solve, readInput, writeOutput)
import Day3 (_solve, solve, readInput, writeOutput)
import Day4 (_solve, solve, readInput, writeOutput)
import Day5 (_solve, solve, readInput, writeOutput)
import Day6 (_solve, solve, readInput, writeOutput)
import Day7 (_solve, solve, readInput, writeOutput)


main :: IO ()
main = do
    -- Day1._solve
    -- Day2._solve
    -- Day3._solve
    -- Day4._solve
    -- Day5._solve
    -- Day6._solve
    Day7._solve
    putStrLn "\n\n✨ All Done! ✨"

-- writeOutput = unlines . (map show)

-- main :: IO ()
-- main = interact (Day1.writeOutput . Day1.solve . Day1.readInput)
-- main = interact (Day2.writeOutput . Day2.solve . Day2.readInput)
-- main = interact (Day3.writeOutput . Day3.solve . Day3.readInput)

