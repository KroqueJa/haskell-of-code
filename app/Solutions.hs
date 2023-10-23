{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}

module Solutions where

{- ========== Imports =========== -}
import Data.List.Split (splitOn)
import Data.List (any, isInfixOf, foldl')
import Data.Array
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Regex.PCRE
import Text.Parsec
import Text.Parsec.String (Parser)
import Foreign.C.String

import Foreign.C.Types
foreign import ccall "solve" c_solve :: CString -> CString -> IO CInt

{- ========== Types ========== -}
type Solver = [String] -> IO String

data InputWrapper = InputWrapper {
                      inputPath :: FilePath,
                      solver :: Solver
                    }

{- ========== Wrapping function for solving 2015 ========== -}
solve2015 :: [InputWrapper] -> IO [String]
solve2015 wrappedInputs = do
  let solverActions = fmap applySolver wrappedInputs
  sequence solverActions
  where
    applySolver :: InputWrapper -> IO String
    applySolver (InputWrapper path solverFn) = do
      content <- readFile path
      let linesOfContent = lines content
      solverFn linesOfContent


-- ======= 2015 Day 1 =======
type Floor = Int

solve2015Day1 :: Solver
solve2015Day1 (line:_) =
  let
    positions = floorPositions line
    partOne = endingFloor positions
    partTwo = firstBasementPosition positions
  in
    return $ "===== Day 1 =====\nPart 1: "
      ++ show partOne ++ "\nPart 2: " ++ show partTwo ++ "\n"
  where
    changeFloor :: Char -> Floor
    changeFloor '(' = 1
    changeFloor ')' = -1
    changeFloor _ = 0

    floorPositions :: String -> [Floor]
    floorPositions = scanl (+) 0 . map changeFloor

    endingFloor :: [Floor] -> Floor
    endingFloor = last

    firstBasementPosition :: [Floor] -> Int
    firstBasementPosition = length . takeWhile (>= 0)

-- ========== 2015 Day 2 ==========

data Box = Box { boxLength :: Int, boxWidth :: Int, boxHeight :: Int } deriving (Show)

solve2015Day2 :: Solver
solve2015Day2 lines =
  let
    tripLists :: [[Int]]
    tripLists = map parseDimensions lines

    boxes :: [Box]
    boxes = map constructBox tripLists

    paperNeededForBoxes = sum $ map paperNeededForBox boxes
    ribbonNeededForBoxes = sum $ map ribbonNeededForBox boxes
  in
    return $ "===== Day 2 =====\nPart 1: "
      ++ show paperNeededForBoxes ++ "\nPart 2: " ++ show ribbonNeededForBoxes ++ "\n"
  where
    parseDimensions :: String -> [Int]
    parseDimensions = map read . splitOn "x"

    constructBox :: [Int] -> Box
    constructBox [l, w, h] = Box { boxLength = l, boxWidth = w, boxHeight = h }
    constructBox _ = error "Wrong length of list passed to constructBox"

    smallestSideArea :: Box -> Int
    smallestSideArea (Box l w h) =
      let
        lw = l*w
        lh = l*h
        wh = w*h
      in
        minimum [lw, lh, wh]

    paperNeededForBox :: Box -> Int
    paperNeededForBox (Box l w h) =
      let slack = smallestSideArea (Box l w h)
      in 2*l*w + 2*w*h + 2*h*l + slack

    shortestPerimeter :: Box -> Int
    shortestPerimeter (Box l w h) =
      let
        a = 2*l + 2*w
        b = 2*l + 2*h
        c = 2*w + 2*h
      in
        minimum [a, b, c]

    volumeOfBox :: Box -> Int
    volumeOfBox (Box l w h) = l*w*h

    ribbonNeededForBox :: Box -> Int
    ribbonNeededForBox b = shortestPerimeter b + volumeOfBox b

-- ========== 2015 Day 3 ==========
data House = House { north :: Int, east :: Int } deriving (Show)

instance Eq House where
  (House n1 e1) == (House n2 e2) = n1 == n2 && e1 == e2

instance Ord House where
  compare (House n1 e1) (House n2 e2)
    | n1 < n2   = LT
    | n1 > n2   = GT
    | e1 < e2   = LT
    | e1 > e2   = GT
    | otherwise = EQ


solve2015Day3 :: Solver
solve2015Day3 (line:_) =
  let
    partOneVisitedHouses = scanl moveSanta (House 0 0) line
    partOneVisitedSet = S.fromList partOneVisitedHouses
    partOne = S.size partOneVisitedSet

    santaMoves = [x | (x, i) <- zip line [0..], i `mod` 2 == 0]
    roboMoves = [x | (x, i) <- zip line [0..], i `mod` 2 /= 0]

    santaVisitedHouses = scanl moveSanta (House 0 0) santaMoves
    roboVisitedHouses = scanl moveSanta (House 0 0) roboMoves

    santaVisitedSet = S.fromList santaVisitedHouses
    roboVisitedSet = S.fromList roboVisitedHouses

    totalVisitedHouses = S.union santaVisitedSet roboVisitedSet

    partTwo = S.size totalVisitedHouses

  in
    return $ "===== Day 3 =====\nPart 1: "
      ++ show partOne ++ "\nPart 2: " ++ show partTwo ++ "\n"
  where
    moveSanta :: House -> Char -> House
    moveSanta (House n e) '^' = House (n+1) e
    moveSanta (House n e) '>' = House n (e+1)
    moveSanta (House n e) 'v' = House (n-1) e
    moveSanta (House n e) '<' = House n (e-1)


-- ========== 2015 Day 4 ==========

{-
I made a haskell-only solution for this problem, but it turns out that the solution
relies heavily on the speed of the MD5 library in order to run in a reasonable time frame.
Even calling a function implemented in C was too slow (though it came up with the
correct solution) so I implemented the full solver in C.
Prior to this I attempted to multi thread the solution in haskell, but did not have the
patience to tweak it. Sorry!
-}
solve2015Day4 :: Solver
solve2015Day4 (line:_) = do
  partOne <- withCString line $ \lineStr ->
             withCString "00000" $ \checkStr ->
             c_solve lineStr checkStr
  partTwo <- withCString line $ \lineStr ->
             withCString "000000" $ \checkStr ->
             c_solve lineStr checkStr
  return $ "===== Day 4 =====\nPart 1: "
    ++ show (fromIntegral partOne :: Int) ++ "\nPart 2: "
    ++ show (fromIntegral partTwo :: Int) ++ "\n"


-- ========== 2015 Day 5 ==========

solve2015Day5 :: Solver
solve2015Day5 lines =
  let
    partOneRegexes =  [
                        "(.*[aeiou].*){3,}",  -- three vowels
                        "(.)\\1",             -- a pair of the same
                        "^(?:(?!ab).)*$",     -- not `ab`
                        "^(?:(?!cd).)*$",     -- not `cd`
                        "^(?:(?!pq).)*$",     -- not `pq`
                        "^(?:(?!xy).)*$"      -- not `xy`
                      ]
    partTwoRegexes =  [
                        "(..).*\\1",
                        "(.).\\1"             -- a char surrounded by a pair
                      ]
    partOne = length $ applyRegexes partOneRegexes lines
    partTwo = length $ applyRegexes partTwoRegexes lines
  in
    return $ "===== Day 5 =====\nPart 1: "
      ++ show partOne ++ "\nPart 2: " ++ show partTwo ++ "\n"
  where
    applyRegexes :: [String] -> [String] -> [String]
    applyRegexes regexes strings = foldl applySingleRegex strings regexes

    applySingleRegex :: [String] -> String -> [String]
    applySingleRegex ss regex = filter (=~ regex) ss

-- ========== 2015 Day 6 ==========

-- Represent a turned on light with a coordinate
type Light = (Int, Int)

type LightSet = S.Set Light
type LightMap = M.Map Light Int

class LightOperation c where
  toggle :: c -> Light -> c
  turnOn :: c -> Light -> c
  turnOff :: c -> Light -> c
  toggleLights :: c -> [Light] -> c
  turnOnLights :: c -> [Light] -> c
  turnOffLights :: c -> [Light] -> c

instance LightOperation (LightSet) where
  toggle lset light = if S.member light lset then S.delete light lset else S.insert light lset
  turnOn = flip S.insert
  turnOff = flip S.delete
  toggleLights lset light = foldl toggle lset light
  turnOffLights lset light = foldl turnOff lset light
  turnOnLights lset light = foldl turnOn lset light

instance LightOperation (LightMap) where
  toggle lmap light = M.insertWith (+) light 2 lmap
  turnOn lmap light = M.insertWith (+) light 1 lmap
  turnOff lmap light = M.alter decreaseToNil light lmap
    where
      decreaseToNil :: Maybe Int -> Maybe Int
      decreaseToNil Nothing = Nothing
      decreaseToNil (Just n) = if n > 0 then Just (n - 1) else Nothing
  toggleLights lmap light = foldl toggle lmap light
  turnOffLights lmap light = foldl turnOff lmap light
  turnOnLights lmap light = foldl turnOn lmap light


-- An input is represented as an `Instruction`, with a helper function to apply it
data FunctionTag = Toggle | TurnOn | TurnOff | None deriving (Show)

data LightOperation c => Instruction c = Instruction {
                        function :: c -> [Light] -> c,
                        tag :: FunctionTag,
                        topLeft :: Light,
                        botRight :: Light
                      }

applyInstruction :: LightOperation c => c -> Instruction c -> c
applyInstruction lset (Instruction f _ tl br) = f lset (rectangle tl br)
  where
    rectangle :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
    rectangle (x, y) (p, q) = [(i, j) | i <- [x..p], j <- [y..q]]


solve2015Day6 :: Solver
solve2015Day6 lines =
  let
    -- An empty set
    lightSet :: LightSet
    lightSet = S.empty

    -- An empty map
    lightMap :: LightMap
    lightMap = M.empty

    -- All instructions
    instructionsPartOne :: [Instruction LightSet]
    instructionsPartOne = parseInstructions lines

    instructionsPartTwo :: [Instruction LightMap]
    instructionsPartTwo = parseInstructions lines

    partOne = tallyLit $ foldl' applyInstruction lightSet instructionsPartOne
    partTwo = sumBrightness $ foldl' applyInstruction lightMap instructionsPartTwo
  in
    return $ "===== Day 6 =====\nPart 1: "
      ++ show partOne ++ "\nPart 2: " ++ show partTwo ++ "\n"
  where
    -- Function to get all the instructions from the input file
    parseInstructions :: LightOperation c => [String] -> [Instruction c]
    parseInstructions = map parseLine
      where
        parseLine :: LightOperation c => String -> Instruction c
        parseLine str = 
          case parse instructionParser "" str of
            Left err -> error $ "Parse error: " ++ show err
            Right instr -> instr

    tallyLit :: LightSet -> Int
    tallyLit = S.size

    sumBrightness :: LightMap -> Int
    sumBrightness = sum . M.elems

    -- Parser functions
    coordinate :: Parser Light
    coordinate = do
      x <- int
      char ','
      y <- int
      return (x, y)
      where
        int :: Parser Int
        int = read <$> many1 digit

    toggleParser :: LightOperation c => Parser (Instruction c)
    toggleParser = do
      string "toggle "
      start <- coordinate
      string " through "
      end <- coordinate
      return $ Instruction toggleLights Toggle start end

    turnOnParser :: LightOperation c => Parser (Instruction c)
    turnOnParser = do
      string "turn on "
      start <- coordinate
      string " through "
      end <- coordinate
      return $ Instruction turnOnLights TurnOn start end

    turnOffParser :: LightOperation c => Parser (Instruction c)
    turnOffParser = do
      string "turn off "
      start <- coordinate
      string " through "
      end <- coordinate
      return $ Instruction turnOffLights TurnOff start end

    instructionParser :: LightOperation c => Parser (Instruction c)
    instructionParser = try toggleParser <|> try turnOnParser <|> turnOffParser

  
