{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Data.Attoparsec.Text qualified as APT
import Data.Attoparsec.Text (Parser, string, inClass, many', satisfy, parse, feed, digit, space, Result)
import Data.Functor
import Control.Applicative
import Data.Tree (Tree)
import Data.Graph (Tree(Node))

main :: IO ()
main = do
  input <- T.IO.readFile "../../input/day7.txt"
  let instructions = (traverse (finalizeParse . parse parseInstruction) . T.lines) input
  let tree = instructions >>=  buildFileSystem (Path []) (emptyFSDir (DirectoryName "/")) . drop 1
  let sizeTree = convertTree <$> tree
  print $ "One: " ++ show (taskOneCalculation <$> sizeTree)
  print $ "Two: " ++ show (taskTwoCalculation <$> sizeTree)

{- 
 - I had to fight with trees but in the end I learned something (?!?)
-}

newtype DirectoryName = DirectoryName Text deriving (Show, Eq)
newtype FileName = FileName Text deriving (Show, Eq)
newtype FileSize = FileSize Int deriving (Show, Eq)

data FSComponent = FSDir DirectoryName | FSFile FileSize FileName deriving (Show, Eq)
data SizeType = FileSizeType Int | DirectorySizeType Int deriving (Show)
type FileSystem = Tree FSComponent
type FileSystemSizes = Tree SizeType

newtype Path = Path [FSComponent] deriving (Show)

taskOneCalculation :: FileSystemSizes -> Int
taskOneCalculation = sum . filter (<= 100000) . fmap getDirSize . flattenTree

taskTwoCalculation :: FileSystemSizes -> Int 
taskTwoCalculation (Node val rest) = (minimum . filter (>= needed) . fmap getDirSize . flattenTree) (Node val rest)
  where 
    available = 70000000 - getSize val
    wanted = 30000000
    needed = wanted - available

flattenTree :: Tree a -> [a]
flattenTree (Node val rest) = val : concatMap flattenTree rest

getFSComponentSize :: FSComponent -> Int
getFSComponentSize (FSDir _) = 0
getFSComponentSize (FSFile (FileSize size) _)= size

getDirSize :: SizeType -> Int 
getDirSize (FileSizeType _) = 0 
getDirSize (DirectorySizeType size ) = size

getSize :: SizeType -> Int
getSize (FileSizeType s) = s
getSize (DirectorySizeType s) = s

convertTree :: FileSystem -> FileSystemSizes
convertTree (Node val []) = Node (FileSizeType $ getFSComponentSize val) []  
convertTree (Node _ rest) = Node (DirectorySizeType dirSize) convertedTree
  where
    convertedTree = convertTree <$> rest
    dirSize = sum (getSize . nodeValue <$> convertedTree)

emptyFSDir :: DirectoryName -> FileSystem
emptyFSDir dirName = Node (FSDir dirName) []

emptyFSFile :: FileSize -> FileName -> FileSystem
emptyFSFile fileSize fileName = Node (FSFile fileSize fileName) []

extractFind :: (a -> Bool) -> [a] -> [a] -> Either [a] (a, [a])
extractFind predf notFound (x:xs) = if predf x then Right (x, notFound ++ xs) else extractFind predf (x : notFound) xs
extractFind _ notFound [] = Left notFound

nodeValue :: Tree a -> a
nodeValue (Node val _) = val

updateFSComponent :: Path -> (FileSystem -> FileSystem) -> FileSystem -> Either String FileSystem
updateFSComponent (Path []) updateF fs = Right (updateF fs)
updateFSComponent (Path (x:xs)) updateF (Node current restfs) = do
  let foundResult = extractFind ((==x) . nodeValue) [] restfs
  case foundResult of
    Left _ -> Left "couldn't resolve path"
    Right (updateFS, nonUpdateFs) -> do
      subResult <- updateFSComponent (Path xs) updateF updateFS
      return (Node current (subResult : nonUpdateFs))

addDirectory :: DirectoryName -> FileSystem -> FileSystem
addDirectory dirName (Node val rest) =  Node val (emptyFSDir dirName : rest)

addFile :: FileSize -> FileName -> FileSystem -> FileSystem
addFile fileSize fileName (Node val rest) =  Node val (emptyFSFile fileSize fileName : rest)

buildFileSystem :: Path ->  FileSystem -> [Instructions] -> Either String FileSystem
buildFileSystem (Path ps) currentFS (i:is)  = case i of
  CD newp -> do
    let newPath = if newp == DirectoryName ".." then Path (drop 1 ps) else Path (FSDir newp : ps)
    buildFileSystem newPath currentFS is
  LS -> do buildFileSystem (Path ps) currentFS is
  Dir dirName -> do
    res <- updateFSComponent (Path (reverse ps)) (addDirectory dirName) currentFS
    buildFileSystem (Path ps) res is
  File fileSize fileName -> do
    res <- updateFSComponent (Path (reverse ps)) (addFile fileSize fileName) currentFS
    buildFileSystem (Path ps) res is
buildFileSystem _ currentFS [] = Right currentFS

data Instructions =
  CD DirectoryName
  | LS
  | Dir DirectoryName
  | File FileSize FileName
  deriving (Show)

finalizeParse :: Result Instructions -> Either String Instructions
finalizeParse = parsedToResult . flip feed ""

parsedToResult :: Result Instructions -> Either String Instructions
parsedToResult = \case
  APT.Done _ r -> Right r
  _ -> Left "Error while parsing"

parseInstruction :: Parser Instructions
parseInstruction = parseCD <|> parseLS <|> parseDir <|> parseFile

isCharacterInName :: Char -> Bool
isCharacterInName = inClass  "./a-zA-Z0-9"

parseCharacterInName :: Parser Char
parseCharacterInName = satisfy isCharacterInName

parseNonWhitespace :: Parser Text
parseNonWhitespace = T.pack <$> many' (satisfy isCharacterInName)

parseNumber :: Parser Int
parseNumber = read <$> many' digit

parseCD :: Parser Instructions
parseCD = string "$ cd" *> space *>(CD . DirectoryName <$> parseNonWhitespace)

parseLS :: Parser Instructions
parseLS = string "$ ls" $> LS

parseDir :: Parser Instructions
parseDir = string "dir" *> space *> (Dir . DirectoryName <$> parseNonWhitespace)

parseFile :: Parser Instructions
parseFile = do
  filesize <- FileSize <$> parseNumber
  _ <- space
  filename <- FileName <$> parseNonWhitespace
  return $ File filesize filename
