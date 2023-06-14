{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse (completeParse) where

import Control.Applicative
import Data.Attoparsec.Text (Parser, decimal, feed, parse, signed, space, string)
import Data.Attoparsec.Text qualified as APT
import Data.Functor
import Data.Text (Text)
import Instruction (Instruction (Addx, Noop))

instructionParser :: Parser Instruction
instructionParser = noopParser <|> addxParser
  where
    noopParser = string "noop" $> Noop
    addxParser = Addx <$> (string "addx" *> space *> signed decimal)

completeParse :: Text -> Either String Instruction
completeParse text = case parsed text of
  APT.Done _ r -> Right r
  _ -> Left "Parse failed"
  where
    parsed = flip feed "" . parse instructionParser
