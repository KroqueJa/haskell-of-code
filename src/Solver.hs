module Solver where

type Solver = [String] -> IO String

data InputWrapper = InputWrapper {
                        inputPath :: FilePath,
                        solver :: Solver
                      }

formatSolution :: String -> String -> String -> String
formatSolution day partOne partTwo = 
    "========== " ++ day ++ "==========\nPart 1: "
      ++ partOne ++ "\nPart 2: " ++ partTwo ++ "\n"

applySolver :: InputWrapper -> IO ()
applySolver (InputWrapper path solverFn) = do
  content <- readFile path
  let linesOfContent = lines content
  result <- solverFn linesOfContent
  putStrLn result

