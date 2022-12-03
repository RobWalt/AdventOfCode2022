{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Set as S
import Data.Char (ord)
import Data.List.Split as LS

main :: IO ()
main = do
  input <- fmap T.pack . readFile $ "../../input/day3.txt"
  day3SolutionTaskOne input
  day3SolutionTaskTwo input

-- utils

printResult :: Show a => Text -> Either String a -> IO ()
printResult task_no res = case res of
  Right ok -> putStrLn $ show task_no ++ " Result is: " ++ show ok
  Left err -> putStrLn $ show task_no ++ " Error: " ++ err

charToScore :: Char -> Either String Int
charToScore c
  | ord c >= ord 'a' && ord c <= ord 'z' = Right $ ord c - ord 'a' + 1
  | ord c >= ord 'A' && ord c <= ord 'Z' = Right $ ord c - ord 'A' + 27
  | otherwise = Left "Unexpected item"

-- task 1

day3SolutionTaskOne :: Text -> IO ()
day3SolutionTaskOne input = printResult "One" . fmap sum $ traverse (charToScore . findItemDifference . splitInHalfs) $ T.lines input

halfLength :: Text -> Int
halfLength = flip div 2 . T.length

splitInHalfs :: Text -> (Text, Text)
splitInHalfs text = T.splitAt (halfLength text)  text

findItemDifference :: (Text, Text) -> Char
findItemDifference (big, small) = S.elemAt 0 $ S.intersection big_chars small_chars
  where
    to_char_set = S.fromList . T.unpack
    big_chars = to_char_set big
    small_chars = to_char_set small

-- task 2

day3SolutionTaskTwo :: Text -> IO ()
day3SolutionTaskTwo = printResult "Two" . fmap sum . itemSets
  where
    mkGroups input = LS.chunksOf 3 (T.lines input)
    findCommonBatch = S.elemAt 0 . foldr1 S.intersection . fmap (S.fromList . T.unpack)
    batchScore = charToScore . findCommonBatch
    itemSets = traverse batchScore . mkGroups
