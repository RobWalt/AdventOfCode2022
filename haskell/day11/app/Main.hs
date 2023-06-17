module Main where

import Control.Arrow (ArrowChoice (left))
import qualified Data.Attoparsec.Text as P
import Data.List (sort)
import MyLib (readAdventFile)
import Parse (parseMonkeys)
import RunTime (fullMonkeyIteration, monkeyIteration, monkeyLCM, oneFullMonkeyIteration)
import Types (Monkey (itemCounter))

main :: IO ()
main = do
  input <- readAdventFile "../../input/day11.txt"
  -- input <- readAdventFile "test.txt"
  let monkeys = P.parseOnly parseMonkeys input
  print monkeys
  print ""
  print "solution 0"
  print ""
  print $ solution0 <$> monkeys
  print ""
  print "solution 1"
  print ""
  print $ solution1 <$> monkeys
  print $ monkeyLCM <$> monkeys

iterNRounds :: Maybe Int -> Int -> [Monkey] -> [Monkey]
iterNRounds lcmNum n mss = foldr (\_ ms -> fullMonkeyIteration lcmNum ms) mss [1 :: Int .. n]

solution0 :: [Monkey] -> Int
solution0 = product . take 2 . reverse . sort . fmap itemCounter . iterNRounds Nothing 20

solution1 :: [Monkey] -> Int
solution1 ms = product . take 2 . reverse . sort . fmap itemCounter . iterNRounds (Just $ monkeyLCM ms) 10000 $ ms
