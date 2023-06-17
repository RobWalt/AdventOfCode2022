module RunTime (fullMonkeyIteration, oneFullMonkeyIteration, monkeyIteration, monkeyLCM) where

import Control.Arrow (ArrowChoice (left))
import Control.Monad
import Data.Bool (bool)
import Data.Foldable (find)
import Data.Maybe (isJust)
import Types (Monkey (falseTargetMonkey, itemCounter, monkeyNumber, operation, startingItems, throwCheck, throwCheckNum, trueTargetMonkey))

monkeyLCM :: [Monkey] -> Int
monkeyLCM = foldToLCM . getCheckNums

foldToLCM :: [Int] -> Int
foldToLCM = foldl lcm 1

getCheckNums :: [Monkey] -> [Int]
getCheckNums = fmap throwCheckNum

fullMonkeyIteration :: Maybe Int -> [Monkey] -> [Monkey]
fullMonkeyIteration lcmNum ms = foldl (flip (oneFullMonkeyIteration lcmNum)) ms (fst <$> [0 ..] `zip` ms)

oneFullMonkeyIteration :: Maybe Int -> Int -> [Monkey] -> [Monkey]
oneFullMonkeyIteration lcmNum idx ms = either id (oneFullMonkeyIteration lcmNum idx) (monkeyIteration lcmNum idx ms)

monkeyIteration :: Maybe Int -> Int -> [Monkey] -> Either [Monkey] [Monkey]
monkeyIteration lcmNum idx ms = left (const ms) $ findMonkey idx ms >>= isMonkeyEmpty >>= (Right . updateValidMonkey lcmNum idx ms)

updateValidMonkey :: Maybe Int -> Int -> [Monkey] -> Monkey -> [Monkey]
updateValidMonkey lcmNum idx ms m = updatedMonkeys
  where
    inspectedMonkey = inspectFirstItem (isJust lcmNum) m
    (checkSuccess, checkedMonkey) = checkFirstItem lcmNum inspectedMonkey
    target = pickThrowTarget checkSuccess inspectedMonkey
    (item, updatedMonkey) = throwItem checkedMonkey
    receivedMonkeys = receiveItem ms target item
    updatedMonkeys = map monkeyCheckReplace receivedMonkeys
      where
        monkeyCheckReplace = join $ bool id (const updatedMonkey) . isMonkeyCurrentMonkey
        isMonkeyCurrentMonkey = (== idx) . monkeyNumber

findMonkey :: Int -> [Monkey] -> Either () Monkey
findMonkey idx ms = maybe (Left ()) Right (find ((== idx) . monkeyNumber) ms)

isMonkeyEmpty :: Monkey -> Either () Monkey
isMonkeyEmpty m = bool (Right m) (Left ()) . (null . startingItems) $ m

inspectFirstItem :: Bool -> Monkey -> Monkey
inspectFirstItem cond = bool applyBoredFirstItem id cond . applyOpFirstItem

applyOpFirstItem :: Monkey -> Monkey
applyOpFirstItem m = changedMonkey
  where
    changedMonkey = m {startingItems = newStartingItems}
    oldItems = startingItems m
    monkeyOp = operation m
    newStartingItems = (monkeyOp . head $ oldItems) : tail oldItems

applyBoredFirstItem :: Monkey -> Monkey
applyBoredFirstItem m = changedMonkey
  where
    changedMonkey = m {startingItems = newStartingItems}
    oldItems = startingItems m
    newStartingItems = ((`div` 3) . head $ oldItems) : tail oldItems

checkFirstItem :: Maybe Int -> Monkey -> (Bool, Monkey)
checkFirstItem lcmNum m = (success, checkedMonkey)
  where
    success = throwCheck m . head . startingItems $ m
    list = startingItems m
    applyLCMToFirst = (maybe id (\n -> (`mod` n)) lcmNum . head $ list) : tail list
    checkedMonkey = m {startingItems = applyLCMToFirst}

pickThrowTarget :: Bool -> Monkey -> Int
pickThrowTarget = bool falseTargetMonkey trueTargetMonkey

throwItem :: Monkey -> (Int, Monkey)
throwItem m = (firstItem, updatedMonkey)
  where
    currentItems = startingItems m
    firstItem = head currentItems
    updatedMonkey = m {startingItems = tail currentItems, itemCounter = itemCounter m + 1}

receiveItem :: [Monkey] -> Int -> Int -> [Monkey]
receiveItem ms target item = fmap replaceIfTarget ms
  where
    replaceIfTarget m = if monkeyNumber m == target then m {startingItems = startingItems m ++ [item]} else m
