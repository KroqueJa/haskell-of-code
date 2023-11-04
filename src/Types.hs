{-# LANGUAGE FlexibleInstances #-}
module Types where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word

{- ======== 2015 Day 2 ======== -}
data Box = Box { boxLength :: Int, boxWidth :: Int, boxHeight :: Int } deriving (Show)

{- ======== 2015 Day 3 ======== -}
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

{- ======== 2015 Day 6 ======== -}
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

{- ======== 2015 Day 7 ======== -}
data Node = Ref String | Const Word16 deriving (Show, Eq, Ord)
data Operation = AND Node Node
                  | OR Node Node
                  | NOT Node
                  | LSHIFT Node Node
                  | RSHIFT Node Node
                  | INPUT Node
  deriving (Show, Eq, Ord)

type CircuitNode = (Node, Operation)
type Circuit = M.Map Node Operation
type Memo = M.Map Node Word16


