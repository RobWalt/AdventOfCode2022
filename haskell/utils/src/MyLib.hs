{-# LANGUAGE ImportQualifiedPost #-}

module MyLib (readAdventFile) where

import Data.Text (Text)
import Data.Text.IO qualified as T.IO

readAdventFile :: String -> IO Text
readAdventFile = T.IO.readFile
