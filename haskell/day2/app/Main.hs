module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad ((>=>))

main :: IO ()
main = do 
  input <- readFile "../../input/day2.txt"
  let result = day2Solution input solveTask1
  case result of 
    Left err -> putStrLn $ "Task1: Encountered error: " ++ err
    Right score -> putStrLn $ "Task1: The strategy yieled a score of " ++ show score
  let result = day2Solution input solveTask2
  case result of 
    Left err -> putStrLn $ "Task2: Encountered error: " ++ err
    Right score -> putStrLn $ "Task2: The strategy yieled a score of " ++ show score


day2Solution :: String -> (Text -> Either String (RockPaperScissors, RockPaperScissors)) -> Either String Int
day2Solution input solve = sum <$> traverse (fmap resultingScore . solve) lines
  where lines = T.lines (T.pack input)

data RockPaperScissors = Rock | Paper | Scissors deriving (Show)
data GameOutcome = Win | Draw | Lose deriving (Show)

resultingScore ::(RockPaperScissors, RockPaperScissors)  -> Int
resultingScore (theirs, mine) = case (theirs, mine) of 
  (Rock, Rock) -> 1 + 3
  (Paper,Rock ) -> 1 + 0
  (Scissors, Rock) -> 1 + 6
  (Rock, Paper) -> 2 + 6
  (Paper, Paper) -> 2 + 3
  (Scissors, Paper) -> 2 + 0
  (Rock, Scissors) -> 3 + 0
  (Paper, Scissors) -> 3 + 6
  (Scissors, Scissors) -> 3 + 3

-- util

tuplify2 :: [a] -> Either String (a, a)
tuplify2 list = case list of
  [a,b] -> Right (a,b)
  _ -> Left "The list didn't include exactly 2 elements"

tuplifiedInput :: Text -> Either String (Text, Text)
tuplifiedInput = tuplify2 . filter (not . T.null) . T.splitOn (T.pack " ") 

parseTuple :: (a -> Either String b) -> (a -> Either String c) -> (a,a) -> Either String (b,c)
parseTuple ffst fsnd tuple = do 
  b <- ffst . fst $ tuple
  c <- fsnd . snd $ tuple
  pure (b, c)

-- task 1

solveTask1 :: Text -> Either String (RockPaperScissors, RockPaperScissors)
solveTask1 = tuplifiedInput >=> parseTuple parseRPSChoice parseRPSChoice

parseRPSChoice :: Text -> Either String RockPaperScissors 
parseRPSChoice text  
  | text == T.pack "A" || text == T.pack "X"  = Right Rock
  | text == T.pack "B" || text == T.pack "Y"  = Right Paper
  | text == T.pack "C" || text == T.pack "Z"  = Right Scissors
  | otherwise = Left "Parsing Rock Paper Scissors choice failed"

-- task 2

solveTask2 :: Text -> Either String (RockPaperScissors, RockPaperScissors)
solveTask2 = fmap chooseRightMove . parseOpponentCoiceAndOutcome

parseOpponentCoiceAndOutcome :: Text -> Either String (RockPaperScissors, GameOutcome)
parseOpponentCoiceAndOutcome = tuplifiedInput >=> parseTuple parseOpponentChoice parseMyChoice

parseOpponentChoice :: Text -> Either String RockPaperScissors 
parseOpponentChoice text  
  | text == T.pack "A" = Right Rock
  | text == T.pack "B" = Right Paper
  | text == T.pack "C" = Right Scissors
  | otherwise = Left "Parsing Rock Paper Scissors choice failed"

parseMyChoice :: Text -> Either String GameOutcome
parseMyChoice text  
  | text == T.pack "X" = Right Lose
  | text == T.pack "Y" = Right Draw
  | text == T.pack "Z" = Right Win
  | otherwise = Left "Parsing Game Outcome failed"

moveBasedOnOutcome :: (RockPaperScissors, GameOutcome) -> RockPaperScissors
moveBasedOnOutcome (theirs, outcome) = case (theirs, outcome) of
  (Rock, Lose) -> Scissors
  (Rock, Draw) -> Rock
  (Rock, Win) -> Paper
  (Paper, Lose) -> Rock
  (Paper, Draw) -> Paper
  (Paper, Win) -> Scissors
  (Scissors, Lose) -> Paper
  (Scissors, Draw) -> Scissors
  (Scissors, Win) -> Rock

chooseRightMove :: (RockPaperScissors, GameOutcome) -> (RockPaperScissors, RockPaperScissors)
chooseRightMove (theirs, outcome) = (theirs, moveBasedOnOutcome (theirs, outcome))
