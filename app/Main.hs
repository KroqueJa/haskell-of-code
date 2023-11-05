import Solutions

-- ======= Main =======
{- Uncomment the solvers you wish to run! -}
main :: IO ()
main = do
  let wrappedInputs = [
                        --InputWrapper "y2015d1.txt" solve2015Day1,
                        --InputWrapper "y2015d2.txt" solve2015Day2,
                        --InputWrapper "y2015d3.txt" solve2015Day3,
                        --InputWrapper "y2015d4.txt" solve2015Day4,
                        --InputWrapper "y2015d5.txt" solve2015Day5,
                        --InputWrapper "y2015d6.txt" solve2015Day6,
                        --InputWrapper "y2015d7.txt" solve2015Day7,
                        --InputWrapper "y2015d8.txt" solve2015Day8,
                        InputWrapper "y2015d9.txt" solve2015Day9
                      ]
  putStrLn "\n\n<<<<<<<<<< ======= AoC 2015 ======= >>>>>>>>>>\n\n"
  results <- solve2015 wrappedInputs
  mapM_ putStrLn results

