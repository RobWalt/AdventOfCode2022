{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Data.Matrix qualified as M
import Data.Matrix (Matrix)
import Data.Vector qualified as V
import Data.Vector (Vector)

main :: IO ()
main = do 
  input <- T.IO.readFile "../../input/day8.txt"
  let treeMatrix = M.fromLists $ fmap digitToInt . T.unpack <$> T.lines input
  print $ "Task One: " ++ show (taskOneSolution treeMatrix)
  print $ "Task Two: " ++ show (taskTwoSolution treeMatrix)
  
digitToInt :: Char -> Int
digitToInt = read . (: [])

maximumOrDefault :: Ord a => a -> Vector a -> a
maximumOrDefault d v= if V.length v == 0 then d else V.maximum v

maximumOrNegative :: Vector Int -> Int
maximumOrNegative = maximumOrDefault (-1)

getIndices :: Matrix Int -> Vector (Int, Int)
getIndices matrix = V.fromList [(row_idx, col_idx)| row_idx <- [1..(M.nrows matrix)], col_idx <- [1..(M.ncols matrix)]]

taskOneSolution :: Matrix Int -> Int
taskOneSolution treeMatrix = V.length . V.filter id . V.map (isTreeVisible treeMatrix) $ getIndices treeMatrix

splitVecAt :: Int -> Vector a -> (Vector a, Vector a)
splitVecAt split_idx v = (V.take (split_idx - 1) v, V.drop split_idx v)

isTreeVisible ::  Matrix Int -> (Int, Int) ->  Bool
isTreeVisible treeMap (row_idx, col_idx) = visibleFromLeft || visibleFromRight || visibleFromTop || visibleFromBottom
  where 
    chosenVal = M.getElem row_idx col_idx treeMap
    rowTrees = M.getRow row_idx treeMap
    colTrees = M.getCol col_idx treeMap
    (leftRow, rightRow) = splitVecAt col_idx rowTrees
    (topCol, bottomCol) = splitVecAt row_idx colTrees
    evalVisibility = (chosenVal >) . maximumOrNegative
    visibleFromLeft = evalVisibility leftRow
    visibleFromRight = evalVisibility rightRow
    visibleFromTop = evalVisibility topCol
    visibleFromBottom = evalVisibility bottomCol

taskTwoSolution :: Matrix Int -> Int
taskTwoSolution treeMatrix = V.maximum  . V.map (scenicScore treeMatrix) $ getIndices treeMatrix

scenicScore :: Matrix Int -> (Int, Int) -> Int
scenicScore treeMap (row_idx, col_idx) =  totalScore
  where 
    chosenVal = M.getElem row_idx col_idx treeMap
    rowTrees = M.getRow row_idx treeMap
    colTrees = M.getCol col_idx treeMap
    (leftRow, rightRow) = splitVecAt col_idx rowTrees
    (topCol, bottomCol) = splitVecAt row_idx colTrees
    takeBlockingTree (seeing, blocked) = V.concat [seeing , V.take 1 blocked]
    evalScore = V.length . takeBlockingTree . V.break (>= chosenVal)
    leftScore = evalScore . V.reverse $ leftRow
    rightScore = evalScore rightRow
    topScore = evalScore . V.reverse $ topCol
    bottomScore = evalScore bottomCol
    totalScore = product [leftScore, rightScore, topScore, bottomScore]
