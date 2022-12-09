{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T.IO
import qualified Data.Attoparsec.Text as AT
import Data.Attoparsec.Text (Parser, many', space, inClass, digit, satisfy, parse, feed)
import Control.Monad (replicateM)
import Data.List (nub)

main :: IO ()
main = do
  input <- T.IO.readFile "../../input/day9.txt"
  let moves = fmap concat . traverse completeParse $ T.lines input
  let taskOne = moves >>= simulateRope 2
  print taskOne
  let taskTwo = moves >>= simulateRope 10
  print taskTwo

simulateRope :: Int -> [Instruction] -> Either String Int
simulateRope knots insts = resLen
  where
    initTrack = Right (replicate knots (Pos 0 0))
    moves = instructionToMove <$> insts
    res = foldl (updateTrack (knots - 1)) initTrack moves
    resLen = length . nub . drop (knots - 1) <$> res

data Instruction = MoveLeft | MoveRight | MoveUp | MoveDown deriving(Show)

parseTag :: Char -> Either String Instruction
parseTag x = case x of
  'L' -> Right MoveLeft
  'R' -> Right MoveRight
  'U' -> Right MoveUp
  'D' -> Right MoveDown
  _ -> Left $ "Instruction " ++ show x ++ " does not exist"

instructionParser :: Parser (Either String [Instruction])
instructionParser = do
  _ <- many' space
  tag <- parseTag <$> satisfy (inClass "LRUD")
  _ <- many' space
  digits <- read <$> many' digit
  return $ replicateM digits tag

completeParse :: Text -> Either String [Instruction]
completeParse = flattener . flip feed "" . parse instructionParser
  where
    flattener parseRes = case parseRes of
      (AT.Done _ instructionRes) -> instructionRes
      _ ->  Left "Parsing failed"

data Pos = Pos !Int !Int deriving (Show, Eq)
data Distance = Distance !Int !Int deriving Show
data MoveBy = MoveBy !Int !Int deriving Show

posDistance :: Pos -> Pos -> Distance
posDistance (Pos x1 y1) (Pos x2 y2) = Distance (x1 - x2) (y1 - y2)

instructionToMove :: Instruction -> MoveBy
instructionToMove = \case
  MoveLeft -> MoveBy (-1) 0
  MoveRight -> MoveBy 1 0
  MoveUp -> MoveBy 0 1
  MoveDown -> MoveBy 0 (-1)

isInDiscreteCircle :: Int -> Distance -> Bool
isInDiscreteCircle radius (Distance dx dy) = (abs dx == radius && abs dy <= radius) || (abs dy == radius && abs dx <= radius)

distanceToMove :: Distance -> Maybe MoveBy
distanceToMove (Distance dx dy) = if isInDiscreteCircle 2 (Distance dx dy) then Just $ MoveBy (signum dx) (signum dy) else Nothing

movePos :: MoveBy -> Pos -> Pos
movePos (MoveBy dx dy) (Pos x y) = Pos (x + dx) (y + dy)

updateTailHead :: Pos -> Pos -> Maybe Pos
updateTailHead h t = ($ t) . movePos <$> distanceToMove (posDistance h t)

updateTail :: Int -> Pos -> [Pos] -> Either String [Pos]
updateTail 1 h (t:ts) = case updateTailHead h t of
  Just nh -> (:) nh <$> Right (t:ts)
  Nothing -> Right (t:ts)
updateTail n h (t:ts) = case updateTailHead h t of
  Just nh -> (:) nh <$> updateTail (n-1) nh ts
  Nothing -> Right (t:ts)
updateTail n _ ts = Left $ "Tail wasn't long enough " ++ show n ++ " " ++ show ts

updateTrack :: Int -> Either String [Pos] -> MoveBy -> Either String [Pos]
updateTrack tailLen (Right (h:ts)) delta = (:) newHead <$> updateTail tailLen newHead ts
  where
    newHead = movePos delta h
updateTrack _ (Left x) _ = Left x
updateTrack _ _ _ = Left "Invalid state encountered"
