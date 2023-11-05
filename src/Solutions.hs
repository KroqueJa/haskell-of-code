{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances  #-}

module Solutions where

import Parsers
import Types

{- ========== Imports =========== -}
import Data.List.Split (splitOn)
import Data.List (any, isInfixOf, foldl', foldl1, permutations)
import Data.Array
import Data.Either (rights)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Regex.PCRE
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import Data.Bits (complement, shiftR, shiftL, (.|.), (.&.))
import Data.Word
import Debug.Trace (trace)
import Data.Maybe (fromJust)

import Foreign.C.String
import Foreign.C.Types
foreign import ccall "solve" c_solve :: CString -> CString -> IO CInt

{- ========== Types ========== -}
type Solver = [String] -> IO String

data InputWrapper = InputWrapper {
                        inputPath :: FilePath,
                        solver :: Solver
                      }

{- ========== Solution Formatter ========== -}
formatSolution :: String -> String -> String -> String
formatSolution day partOne partTwo = 
    "========== " ++ day ++ "==========\nPart 1: "
      ++ partOne ++ "\nPart 2: " ++ partTwo ++ "\n"

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
    return $ formatSolution "Day 1" (show partOne) (show partTwo)

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


solve2015Day2 :: Solver
solve2015Day2 lines =
  let
    tripLists :: [[Int]]
    tripLists = map parseDimensions lines

    boxes :: [Box]
    boxes = map constructBox tripLists

    paperNeededForBoxes = sum $ map paperNeededForBox boxes
    ribbonNeededForBoxes = sum $ map ribbonNeededForBox boxes

    partOne = show paperNeededForBoxes
    partTwo = show ribbonNeededForBoxes

  in
    return $ formatSolution "Day 2" (show partOne) (show partTwo)

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
    return $ formatSolution "Day 3" (show partOne) (show partTwo)

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
  partOneC <- withCString line $ \lineStr ->
             withCString "00000" $ \checkStr ->
             c_solve lineStr checkStr
  partTwoC <- withCString line $ \lineStr ->
             withCString "000000" $ \checkStr ->
             c_solve lineStr checkStr

  let partOne = show (fromIntegral partOneC :: Int)
  let partTwo = show (fromIntegral partTwoC :: Int)

  return $ formatSolution "Day 4" partOne partTwo

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
    return $ formatSolution "Day 5" (show partOne) (show partTwo)

  where
    applyRegexes :: [String] -> [String] -> [String]
    applyRegexes regexes strings = foldl applySingleRegex strings regexes

    applySingleRegex :: [String] -> String -> [String]
    applySingleRegex ss regex = filter (=~ regex) ss

-- ========== 2015 Day 6 ==========

-- Represent a turned on light with a coordinate
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
    return $ formatSolution "2015 Day 6" (show partOne) (show partTwo)

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


-- ========== 2015 Day 7 ==========

solve2015Day7 :: Solver
solve2015Day7 lines =
  let
    (partOne, _) = evaluateNode (Ref "a") M.empty $ buildCircuit $ parseLines lines
    (partTwo, _) = evaluateNode (Ref "a") M.empty $ M.adjust (\_ -> INPUT (Const partOne)) (Ref "b") $ buildCircuit $ parseLines lines

  in

    return $ formatSolution "2015 Day 7" (show partOne) (show partTwo)

  where

    evaluateNode :: Node -> Memo -> Circuit -> (Word16, Memo)
    evaluateNode node memo circuit =
      case M.lookup node memo of
        Just val -> (val, memo)  -- Memoized value
        Nothing -> 
          let (val, newMemo) = case M.lookup node circuit of
                Nothing -> error $ "Node not found in circuit: " ++ show node
                Just operation -> 
                  evaluateOperation operation memo circuit
          in
            (val, M.insert node val newMemo)

    evaluateOperation :: Operation -> Memo -> Circuit -> (Word16, Memo)
    evaluateOperation (AND left right) memo circuit =
        let (leftVal, memo1) = eval left memo circuit
            (rightVal, memo2) = eval right memo1 circuit
        in (leftVal .&. rightVal, memo2)
    evaluateOperation (OR left right) memo circuit =
        let (leftVal, memo1) = eval left memo circuit
            (rightVal, memo2) = eval right memo1 circuit
        in (leftVal .|. rightVal, memo2)
    evaluateOperation (LSHIFT left right) memo circuit =
        let (leftVal, memo1) = eval left memo circuit
            (rightVal, memo2) = eval right memo1 circuit
        in (leftVal `shiftL` fromIntegral rightVal, memo2)
    evaluateOperation (RSHIFT left right) memo circuit =
        let (leftVal, memo1) = eval left memo circuit
            (rightVal, memo2) = eval right memo1 circuit
        in (leftVal `shiftR` fromIntegral rightVal, memo2)
    evaluateOperation (NOT op) memo circuit =
        let (opVal, newMemo) = eval op memo circuit
        in (complement opVal, newMemo)
    evaluateOperation (INPUT op) memo circuit = eval op memo circuit

    eval :: Node -> Memo -> Circuit -> (Word16, Memo)
    eval (Const x) memo _ = (x, memo)
    eval ref@(Ref _) memo circuit = evaluateNode ref memo circuit

    parseLines :: [String] -> [Either ParseError CircuitNode]
    parseLines lines = map (parse circuitNodeParser "") lines

    buildCircuit :: [Either ParseError CircuitNode] -> Circuit
    buildCircuit parsedList = M.fromList $ map (either handleError id) parsedList
      where
        handleError :: ParseError -> a
        handleError err = error $ "Parsing failed with error: " ++ show err

-- ========== 2015 Day 8 ==========
solve2015Day8 :: Solver
solve2015Day8 lines =
  let
    noSurroundingQuotes :: [String]
    noSurroundingQuotes = map interior lines

    santaStrings :: [SantaString]
    santaStrings = rights $ map (parse santaStringParser "") noSurroundingQuotes

    totalChars :: Int
    totalChars = sum $ map length lines

    totalSantaChars :: Int
    totalSantaChars = sum $ map length santaStrings

    partOne = show $ totalChars - totalSantaChars

    partTwo = show $ sum $ map addedFromQuotesAndSlashes lines

  in
    return $ formatSolution "2015 Day 8" partOne partTwo

  where
    addedFromQuotesAndSlashes :: String -> Int
    addedFromQuotesAndSlashes = (+4) . length . filter (`elem` ['\\', '"']) . interior

    interior :: (Eq a) => [a] -> [a]
    interior = tail . init

-- ========== 2015 Day 9 ==========
solve2015Day9 :: Solver
solve2015Day9 lines =
  let
    -- Parse all information into our datatype
    trips :: [Trip]
    trips = rights $ map (parse tripParser "") lines

    -- Create a distance catalog and a list of unique towns
    distanceCatalog :: DistanceCatalog
    distanceCatalog = buildCatalog $ trips

    uniqueTowns :: [TownID]
    uniqueTowns =
      let
        froms = map (town1 . towns) trips
        tos = map (town2 . towns) trips
        fromSet = S.fromList froms
        toSet = S.fromList tos
      in
        S.toList $ S.union fromSet toSet

    -- Create all possible permutations of the unique towns
    allRoutes :: [Route]
    allRoutes = permutations uniqueTowns

    -- Form town pairs out of each such permutation
    allPairs :: [[TownPair]]
    allPairs = map formPairs allRoutes

    -- For each such town pair, look the distance up in the DistanceCatalog
    allDistances :: [[Int]]
    allDistances = map (calculateDistances distanceCatalog) allPairs

    -- Sum each set of distances
    allTotalDistances :: [Int]
    allTotalDistances = map sum allDistances

    -- The solution to part one is now the minimum distance
    partOne = show $ minimum allTotalDistances

    -- The solution to part two is trivial
    partTwo = show $ maximum allTotalDistances

  in
    return $ formatSolution "2015 Day 9" partOne partTwo

  where
    calculateDistances :: M.Map TownPair Int -> [TownPair] -> [Int]
    calculateDistances m = map (fromJust . (`M.lookup` m))


    formPairs :: Route -> [TownPair]
    formPairs [] = []
    formPairs [t1,t2] = [TownPair t1 t2]
    formPairs (t1:t2:ts) = let p = TownPair t1 t2 in p : formPairs (t2:ts)

    buildCatalog :: [Trip] -> DistanceCatalog
    buildCatalog = M.fromList . map tripToEntry
      where
        tripToEntry :: Trip -> (TownPair, Int)
        tripToEntry (Trip towns d) = (towns, d)

