module Main where

import Data.Text (pack, Text, splitOn, null) 
import Data.Text.Read (decimal, Reader)
import Data.List (sortBy)

main :: IO ()
main = do 
  input <- readFile "../../input/day1.txt"
  mapM_ (day1Solution input) [1,3]

day1Solution :: String -> Int -> IO () 
day1Solution input n = printResult n . fmap (getNTopCalories n) . traverse (fmap sumCalories . parseCalories) . getListPerElf . pack $ input

printResult :: Int -> Either String Integer -> IO () 
printResult n result = do 
  case result of 
    Left err -> putStrLn $  "Error when calculating top " ++ show n ++ " elves calories: " ++ err
    Right max_cals -> putStrLn $ "Calories of top " ++ show n ++ " elves calories: " ++ show max_cals

nonEmptySplitOn :: Text -> Text -> [Text]
nonEmptySplitOn on = filter (not . Data.Text.null) . splitOn on

getListPerElf :: Text -> [Text]
getListPerElf = nonEmptySplitOn (pack "\n\n")

parseCalories :: Text -> Either String [Integer]
parseCalories = traverse (fmap fst . decimal) . nonEmptySplitOn (pack "\n")

sumCalories :: [Integer] -> Integer 
sumCalories = sum

getNTopCalories :: Int -> [Integer] -> Integer
getNTopCalories n = sum . take n . sortBy (flip compare)
