{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad
import Foreign (fromBool)

main :: IO ()
main = do
  input <- T.pack <$> readFile "../../input/day4.txt"
  day4SolutionTaksOne input
  day4SolutionTaksTwo input

printResult :: Show a => Text -> Either String a -> IO ()
printResult task_no res = case res of
  Right ok -> putStrLn $ show task_no ++ " Result is: " ++ show ok
  Left err -> putStrLn $ show task_no ++ " Error: " ++ err

count :: (a-> Bool) -> [a] -> Int
count p = sum . fmap (fromBool . p)

newtype Range = Range (Int, Int) deriving Show

parseRangeParts :: [Text] -> Either String Range
parseRangeParts [start, end] = Right $ Range (read (T.unpack start), read (T.unpack end))
parseRangeParts _ = Left "Error while parsing range"

parseRangeText :: Text -> Either String Range
parseRangeText = parseRangeParts . T.splitOn "-"

toIntervals :: Text -> Either String (Range, Range)
toIntervals =  traverse parseRangeText . T.splitOn "," >=> tuplify2

tuplify2 :: [a] -> Either String (a, a)
tuplify2 [one, two] = Right (one, two)
tuplify2 _ = Left "Error, more than two parts when exactly two were expected"

-- task 1

isIncluded :: Range -> Range -> Bool
isIncluded (Range (start_big, end_big)) (Range (start_small, end_small)) = start_big <= start_small && end_small <= end_big

isOneRangeCompleteInclusion :: (Range, Range) -> Bool
isOneRangeCompleteInclusion (a,b) = isIncluded a b || isIncluded b a

day4SolutionTaksOne :: Text -> IO ()
day4SolutionTaksOne = printResult "Task One" . count_included_intervals
  where
  counting_f = count isOneRangeCompleteInclusion
  parse = traverse toIntervals . T.lines
  count_included_intervals input = counting_f <$> parse input


-- task 2

isPartiallyIncluded :: Range -> Range -> Bool
isPartiallyIncluded (Range (start_big, end_big)) (Range (start_small, end_small)) = (start_big <= start_small && start_small <= end_big) || (start_big <= end_small && end_small <= end_big)

isOneRangePartialInclusion :: (Range, Range) -> Bool
isOneRangePartialInclusion (a,b) = isPartiallyIncluded a b || isPartiallyIncluded b a

day4SolutionTaksTwo :: Text -> IO ()
day4SolutionTaksTwo = printResult "Task Two" . count_partially_included_intervals
  where
  counting_f = count isOneRangePartialInclusion
  parse = traverse toIntervals . T.lines
  count_partially_included_intervals input = counting_f <$> parse input
