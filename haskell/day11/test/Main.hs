{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Attoparsec.Text as P
import Data.Either (isRight)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Parse
  ( parseFalseThrowTarget,
    parseMonkey,
    parseMonkeyCheckNum,
    parseMonkeyNumber,
    parseMonkeyOperation,
    parseMonkeys,
    parseStartingItems,
    parseTrueThrowTarget,
  )
import Test.HUnit (Test (TestCase, TestList), assertBool, runTestTT)

main :: IO ()
main = do
  putStrLn "Running tests ..."
  counts <- runTestTT tests
  print counts

tests :: Test
tests = TestList [t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13]
  where
    t1 = mkTest parseFalseThrowTarget "    If false: throw to monkey 4"
    t2 = mkTest parseMonkeyCheckNum "  Test: divisible by 2"
    t3 = mkTest parseMonkeyNumber "Monkey 0:"
    t4 = mkTest parseMonkeyOperation "Operation: new = old * 11"
    t5 = mkTest parseStartingItems "  Starting items: 89, 95, 92, 64, 87, 68"
    t6 = mkTest parseTrueThrowTarget "    If true: throw to monkey 7"
    t7 = mkTest (parseMonkeyNumber *> parseStartingItems) "Monkey 0:\n  Starting items: 89, 95, 92, 64, 87, 68\n"
    t8 = mkTest (parseMonkeyNumber *> parseStartingItems *> parseMonkeyOperation) "Monkey 0:\n  Starting items: 89, 95, 92, 64, 87, 68\n\nOperation: new = old * 11\n"
    t9 = mkTest (parseMonkeyNumber *> parseStartingItems *> parseMonkeyOperation *> parseMonkeyCheckNum) "Monkey 0:\n  Starting items: 89, 95, 92, 64, 87, 68\nOperation: new = old * 11\n  Test: divisible by 2\n"
    t10 = mkTest (parseMonkeyNumber *> parseStartingItems *> parseMonkeyOperation *> parseMonkeyCheckNum *> parseTrueThrowTarget) "Monkey 0:\n  Starting items: 89, 95, 92, 64, 87, 68\nOperation: new = old * 11\n  Test: divisible by 2\n    If true: throw to monkey 7\n"
    t11 = mkTest (parseMonkeyNumber *> parseStartingItems *> parseMonkeyOperation *> parseMonkeyCheckNum *> parseTrueThrowTarget *> parseFalseThrowTarget) "Monkey 0:\n  Starting items: 89, 95, 92, 64, 87, 68\nOperation: new = old * 11\n  Test: divisible by 2\n    If true: throw to monkey 7\n    If false: throw to monkey 4\n"
    t12 = mkTest parseMonkey ("Monkey 0:\n" <> "  Starting items: 89, 95, 92, 64, 87, 68\n" <> "  Operation: new = old * 11\n" <> "  Test: divisible by 2\n" <> "    If true: throw to monkey 7\n" <> "    If false: throw to monkey 4\n")
    t13 = mkTest parseMonkeys ("Monkey 0:\n" <> "  Starting items: 89, 95, 92, 64, 87, 68\n" <> "  Operation: new = old * 11\n" <> "  Test: divisible by 2\n" <> "    If true: throw to monkey 7\n" <> "    If false: throw to monkey 4\n")

mkTest :: P.Parser a -> T.Text -> Test
mkTest p s = TestCase $ do
  let res = P.parseOnly p s
  assertBool ("The following is left:\n\"" ++ extractError res ++ "\"\n") (isRight res)
  where
    extractError :: Either String a -> String
    extractError = fromMaybe "" . either (const Nothing) Just . either Right Left
