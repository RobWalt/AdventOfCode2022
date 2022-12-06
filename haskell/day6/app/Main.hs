{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (nub)

main :: IO ()
main = do 
  input <- readFile "../../input/day6.txt"
  print $ findPackageStart 4 input
  print $ findPackageStart 14 input

findPackageStart :: Int -> String -> Either String Int
findPackageStart numCharacters = fmap (+numCharacters) . go 0 
  where 
    safeBigTake = (\chars -> if length chars == numCharacters then Right chars else Left "list gets too short | no start found") . take numCharacters
    charactersUnique = (== numCharacters) . length . nub
    go int (x:xs) = safeBigTake (x:xs) >>= (\lh -> if charactersUnique lh then Right int else go (int + 1) xs)
    go _ [] = Left "list empty | no start found"
