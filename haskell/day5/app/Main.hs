{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Attoparsec.Text as APT
import  Data.Attoparsec.Text (Parser, char, letter, space, feed, parse, string, digit, many1)
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad (join, (>=>))
import Data.List

type Stacks = Map Int [Char]

data Instruction = Instruction {
  count :: Int,
  from :: Int,
  to :: Int
} deriving Show

main :: IO ()
main = do
  input <-  T.pack <$> readFile "../../input/day5.txt"
  let splitted = splitStartAndInstructions input
  let result_start_parse = fmap (buildMap . onlyTakeValidChars) (fmap (enumerate . fmap join . sequenceA . flatParseLine packetsParser) . T.lines . fst) <$> splitted
  let result_instructions_parse = mapMaybe (flatParseLine instructionsParser) . T.lines . snd <$> splitted
  print $ "Taks 1: " ++ show (topsOfTheStacks <$> handleResults reverse result_start_parse result_instructions_parse)
  print $ "Taks 2: " ++ show (topsOfTheStacks <$> handleResults id result_start_parse result_instructions_parse)

topsOfTheStacks :: Stacks -> [Char]
topsOfTheStacks stacks = tops
  where
    stackNums = sortBy (flip compare) $ M.keys stacks
    appendHead list num = (flip M.lookup stacks >=> listToMaybe) num : list
    tops = map (fromMaybe ' ') $ foldl appendHead [] stackNums

handleResults :: ([Char] -> [Char]) -> Either String Stacks -> Either String [Instruction] -> Either String Stacks
handleResults stackDirectionF stackResult instructionsResult = do
  stack <- stackResult
  instructions <- instructionsResult
  let newStack = foldl (applyInstruction stackDirectionF) stack instructions
  return newStack

enumerate :: [b] -> [(Int, b)]
enumerate = zip [1..]

onlyTakeValidChars :: [[(a, Maybe b)]] -> [(a,b)]
onlyTakeValidChars = mapMaybe sequenceA . concat

tuplify2 :: [a] -> Either String (a, a)
tuplify2 [one, two] = Right (one, two)
tuplify2 _ = Left "Error, more than two parts when exactly two were expected"

splitStartAndInstructions :: Text -> Either String (Text, Text)
splitStartAndInstructions = tuplify2 . T.splitOn "\n\n"

fullPacketParser :: Parser (Maybe Char)
fullPacketParser = do
  _ <- char '['
  typeOfPacket <- letter
  _ <- char ']'
  return (Just typeOfPacket)

emptyPacketParser :: Parser (Maybe Char)
emptyPacketParser = do
  _ <- space *> space *> space
  return Nothing

packetParser :: Parser (Maybe Char)
packetParser =  (fullPacketParser <|> emptyPacketParser) <* optional space

packetsParser :: Parser [Maybe Char]
packetsParser = many packetParser

instructionsParser :: Parser Instruction
instructionsParser = do
  _ <- string "move"
  _ <- space
  count <- many1 digit
  _ <- space
  _ <- string "from"
  _ <- space
  from <- many1 digit
  _ <- space
  _ <- string "to"
  _ <- space
  to <- many1 digit
  _ <- optional (many space)
  return (Instruction (read count) (read from) (read to))

flatParseLine :: Parser a -> Text -> Maybe a
flatParseLine = fa
  where
    fa parser = fb . flip feed "" . parse parser
    fb res = case res of
      APT.Done _ a -> Just a
      _ -> Nothing

buildMap :: [(Int, Char)] -> Stacks
buildMap = foldr folder M.empty
  where
    folder (idx, c) = M.insertWith (++) idx [c]

applyInstruction :: ([Char] -> [Char]) -> Stacks -> Instruction ->  Stacks
applyInstruction stackDirectionF old_stacks (Instruction { count, from, to }) = new_stacks
  where
    get_crates = splitAt count <$> M.lookup from old_stacks
    update_removed_f _ = snd <$> get_crates
    update_added_f old = (++ old) . stackDirectionF . fst <$> get_crates
    new_stacks = (M.update update_added_f to . M.update update_removed_f from) old_stacks

