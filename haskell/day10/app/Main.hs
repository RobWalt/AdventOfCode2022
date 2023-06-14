{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow ((***), (>>>))
import Data.Bool (bool)
import Data.Text qualified as T
import Instruction (Instruction (Addx, Noop))
import MyLib (readAdventFile)
import Parse (completeParse)

main :: IO ()
main = do
  input <- readAdventFile "../../input/day10.txt"
  let instructions = traverse completeParse (T.lines input)
  let measurements ini = foldl updateWithInstruction ini <$> instructions

  let initialCyclesAndVal1 = [(1, 1)]
  let res1 = solutionTaskOne <$> measurements initialCyclesAndVal1
  print res1

  let initialCyclesAndVal2 = [(0, 1)]
  let res2 = solutionTaskTwo . reverse . tail <$> measurements initialCyclesAndVal2
  mapM_ (mapM_ print) res2

updateWithInstruction :: [(Int, Int)] -> Instruction -> [(Int, Int)]
updateWithInstruction [] _ = []
updateWithInstruction ((c, v) : xs) i = case i of
  Noop -> (c + oneCycle, v) : (c, v) : xs
  Addx dx -> (c + twoCycles, v + dx) : (c + oneCycle, v) : (c, v) : xs
  where
    oneCycle = 1
    twoCycles = 2

solutionTaskOne :: [(Int, Int)] -> Int
solutionTaskOne = sumSignals . filterSignals

sumSignals :: [(Int, Int)] -> Int
sumSignals = sum . map (uncurry (*))

filterSignals :: [(Int, b)] -> [(Int, b)]
filterSignals = filter (\(c, _) -> (c + 20) `mod` 40 == 0)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = take n xs : splitEvery n (drop n xs)

solutionTaskTwo :: [(Int, Int)] -> [String]
solutionTaskTwo = splitEvery 40 . oneLineCRT
  where
    oneLineCRT = fmap (lightUp . checkIsLit)
    lightUp = bool '.' '#'
    checkIsLit = currentPos *** currentSprite >>> uncurry elem
      where
        currentPos = flip mod 40
        currentSprite = flip fmap [-1 .. 1] . (+)
