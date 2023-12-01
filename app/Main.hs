module Main (main) where

import Solver
import Solutions


solve :: [InputWrapper] -> IO ()
solve wrappedInputs = mapM_ applySolver wrappedInputs

main :: IO ()
main = do
  putStrLn "\n\n<<<<<<<<<< ======= AoC 2015 ======= >>>>>>>>>>\n\n"
  solve inputs2015
  putStrLn "\n\n<<<<<<<<<< ======= AoC 2023 ======= >>>>>>>>>>\n\n"
  solve inputs2023
  where
    inputs2015 = [
--                          InputWrapper "y2015d1.txt" solve2015Day1,
--                          InputWrapper "y2015d2.txt" solve2015Day2,
--                          InputWrapper "y2015d3.txt" solve2015Day3,
--                          InputWrapper "y2015d4.txt" solve2015Day4,
--                          InputWrapper "y2015d5.txt" solve2015Day5,
--                          InputWrapper "y2015d6.txt" solve2015Day6,
--                          InputWrapper "y2015d7.txt" solve2015Day7,
--                          InputWrapper "y2015d8.txt" solve2015Day8,
--                          InputWrapper "y2015d9.txt" solve2015Day9,
--                          InputWrapper "y2015d10.txt" solve2015Day10,
--                          InputWrapper "y2015d11.txt" solve2015Day11
                        ]
    inputs2023 = [
                            InputWrapper "y2023d1.txt" solve2023Day1
                        ]
