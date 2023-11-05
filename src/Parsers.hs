module Parsers where

import Types

import qualified Data.Map.Strict as M
import Text.Parsec.String (Parser)
import Text.Regex.PCRE
import Text.Parsec

{- ======== 2015 Day 6 ======== -}

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

{- ======== 2015 Day 7 ======== -}
nodeParser :: Parser Node
nodeParser = try constParser <|> refParser

constParser :: Parser Node
constParser = do
  value <- many1 digit
  return $ Const (read value)

refParser :: Parser Node
refParser = do
  ref <- many1 letter
  return $ Ref ref

notOperationParser :: Parser Operation
notOperationParser = do
  _ <- string "NOT "
  operand <- nodeParser
  return $ NOT operand

andOperationParser :: Parser Operation
andOperationParser = do
  lvalue <- nodeParser
  _ <- string " AND "
  rvalue <- nodeParser
  return $ AND lvalue rvalue

orOperationParser :: Parser Operation
orOperationParser = do
  lvalue <- nodeParser
  _ <- string " OR "
  rvalue <- nodeParser
  return $ OR lvalue rvalue

rshiftOperationParser :: Parser Operation
rshiftOperationParser = do
  lvalue <- nodeParser
  _ <- string " RSHIFT "
  rvalue <- nodeParser
  return $ RSHIFT lvalue rvalue

lshiftOperationParser :: Parser Operation
lshiftOperationParser = do
  lvalue <- nodeParser
  _ <- string " LSHIFT "
  rvalue <- nodeParser
  return $ LSHIFT lvalue rvalue

inputOperationParser :: Parser Operation
inputOperationParser = do
  node <- nodeParser
  return $ INPUT node

operationParser :: Parser Operation
operationParser = try lshiftOperationParser
                    <|> try rshiftOperationParser
                    <|> try orOperationParser
                    <|> try andOperationParser
                    <|> try notOperationParser
                    <|> try inputOperationParser

circuitNodeParser :: Parser CircuitNode
circuitNodeParser = do
  operation <- operationParser
  _ <- string " -> "
  outputNode <- nodeParser
  return (outputNode, operation)

circuitParser :: Parser Circuit
circuitParser = do
  circuitNodes <- many circuitNodeParser
  return $ M.fromList circuitNodes

{- ======== 2015 Day 8 ======== -}
santaStringParser :: Parser SantaString
santaStringParser = many (try santaCharParser)

santaCharParser :: Parser (SantaChar Char)
santaCharParser = try hexParser
                    <|> try escapedParser
                    <|> try plainParser

plainParser :: Parser (SantaChar Char) 
plainParser = do
  c <- noneOf "\\"
  return $ Plain c

escapedParser :: Parser (SantaChar Char)
escapedParser = do
  char '\\'
  c <- noneOf "x"
  return $ Escaped c

hexParser :: Parser (SantaChar Char)
hexParser = do
  char '\\'
  char 'x'
  count 2 anyChar
  return HexValue

{- ======== 2015 Day 9 ======== -}
tripParser :: Parser Trip
tripParser = do
  t1 <- many1 letter
  _ <- string " to "
  t2 <- many1 letter
  _ <- string " = "
  dist <- many1 digit
  return Trip { towns = TownPair { town1 = t1, town2 = t2 }, tripDistance = (read dist) }

