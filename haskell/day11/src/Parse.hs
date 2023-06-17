{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseMonkeys,
    parseMonkey,
    parseMonkeyNumber,
    parseStartingItems,
    parseMonkeyOperation,
    parseMonkeyCheckNum,
    parseTrueThrowTarget,
    parseFalseThrowTarget,
  )
where

import Control.Applicative
import Data.Attoparsec.Text qualified as P
import Data.Bool (bool)
import Data.Either (fromRight)
import Data.Text (Text)
import Types (Monkey (Monkey, falseTargetMonkey, itemCounter, monkeyNumber, operation, startingItems, throwCheck, throwCheckNum, trueTargetMonkey))

parseMonkeys :: P.Parser [Monkey]
parseMonkeys = P.many1 parseMonkey

parseMonkey :: P.Parser Monkey
parseMonkey = do
  num <- parseMonkeyNumber
  list <- parseStartingItems
  op <- parseMonkeyOperation
  checkNum <- parseMonkeyCheckNum
  trueT <- parseTrueThrowTarget
  falseT <- parseFalseThrowTarget
  _ <- optional P.skipSpace
  return
    Monkey
      { monkeyNumber = num,
        startingItems = list,
        operation = op,
        throwCheckNum = checkNum,
        throwCheck = \x -> x `mod` checkNum == 0,
        trueTargetMonkey = trueT,
        falseTargetMonkey = falseT,
        itemCounter = 0
      }

trimWhitespace :: P.Parser a -> P.Parser a
trimWhitespace p = P.skipSpace *> p <* P.skipSpace

--

parseMonkeyNumber :: P.Parser Int
parseMonkeyNumber = trimWhitespace $ P.string "Monkey " *> P.decimal <* P.char ':'

parseStartingItems :: P.Parser [Int]
parseStartingItems = trimWhitespace $ P.string "Starting items: " *> P.many' parseOneItem
  where
    parseOneItem :: P.Parser Int
    parseOneItem = P.decimal <* optional (P.string ", ")

parseMonkeyOperation :: P.Parser (Int -> Int)
parseMonkeyOperation = trimWhitespace $ do
  _ <- P.string "Operation: new = "
  first <- parseOldOrNum
  _ <- P.space
  op <- parseOp
  _ <- P.space
  second <- parseOldOrNum
  return (\old -> fromRight old first `op` fromRight old second)
  where
    parseOp :: P.Parser (Int -> Int -> Int)
    parseOp = decideOp <$> P.anyChar
    decideOp :: Char -> (Int -> Int -> Int)
    decideOp = bool (*) (+) . (== '+')
    parseOldOrNum :: P.Parser (Either Text Int)
    parseOldOrNum = P.choice [Left <$> P.string "old", Right <$> P.decimal]

parseMonkeyCheckNum :: P.Parser Int
parseMonkeyCheckNum = trimWhitespace $ P.string "Test: divisible by " *> P.decimal

parseTrueThrowTarget :: P.Parser Int
parseTrueThrowTarget = trimWhitespace $ P.string "If true: throw to monkey " *> P.decimal

parseFalseThrowTarget :: P.Parser Int
parseFalseThrowTarget = trimWhitespace $ P.string "If false: throw to monkey " *> P.decimal
