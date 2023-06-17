module Types where

data Monkey = Monkey
  { monkeyNumber :: Int,
    startingItems :: [Int],
    operation :: Int -> Int,
    throwCheckNum :: Int,
    throwCheck :: Int -> Bool,
    trueTargetMonkey :: Int,
    falseTargetMonkey :: Int,
    itemCounter :: Int
  }

instance Show Monkey where
  show (Monkey num items op checkNum check tt ft count) =
    "\nMonkey "
      ++ show num
      ++ "\nItems: "
      ++ show items
      ++ "\nOperation (apply 5): "
      ++ show (op 5)
      ++ "\nCheck Num: "
      ++ show checkNum
      ++ "\nCheck (apply 5): "
      ++ show (check 5)
      ++ "\nTarget True: "
      ++ show tt
      ++ "\nTarget False: "
      ++ show ft
      ++ "\nItem Counter: "
      ++ show count
      ++ "\n"
