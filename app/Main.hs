{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8 ( Parser
                                        , takeTill
                                        , string
                                        , skipSpace
                                        , parse
                                        , manyTill
                                        , anyChar
                                        )
import qualified Data.Attoparsec.ByteString.Char8 as A

fp :: FilePath
fp = "/home/decoy/Documents/vimwiki/study/category-theory/terminology.md"

type Modeline = ByteString
type H1 = ByteString
type H2 = ByteString
data Entry = Entry H2 [ByteString]
data Wiki = Wiki Modeline H1 [Entry]

b :: ByteString
b = "# "

bb :: ByteString
bb = "## "

n :: ByteString
n = "\n"

nn :: ByteString
nn = "\n\n"

instance Show Entry where
  show (Entry h2 lines) = show all where
    all = bb <> h2 <> n <> foldr (\acc l -> acc <> n <> l) mempty lines

instance Show Wiki where
  show (Wiki modeline h1 entries) = show all where
    all = modeline <> nn <> b <> h1 <> nn <> foldr (\acc e -> e <> nn) mempty entries

isNewLine :: Char -> Bool
isNewLine = (==) '\n'

--------------------------------------------------------------------------------

parseModeline :: Parser Modeline
parseModeline = do
  x <- manyTill anyChar (string "\n")
  return (B.pack x)

parseH1 :: Parser H2
parseH1 = do
  _ <- string "# "
  heading <- takeTill isNewLine
  return heading

parseH2 :: Parser H2
parseH2 = do
  _ <- string "## "
  heading <- takeTill isNewLine
  return heading

parseEntry :: Parser Entry
parseEntry = do
  h2 <- parseH2
  return (Entry h2 [])

parseWiki :: Parser Wiki
parseWiki = do
  modeline <- parseModeline
  _ <- skipSpace
  h1 <- parseH1
  return (Wiki modeline h1 [])

--------------------------------------------------------------------------------

onSuccess :: Wiki -> IO ()
onSuccess match = do
  putStrLn "Success"
  putStrLn (show match)

onFail :: [String] -> String -> IO ()
onFail contexts message = do
  putStrLn "Fail"
  putStrLn message

onPartial :: IO ()
onPartial = putStrLn "Partial"

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Please provide a filename"
  name <- getLine
  file <- B.readFile fp
  let res = parse parseWiki file
  case res of
    (A.Done rest match) -> onSuccess match
    (A.Fail rest contexts message) -> onFail contexts message
    _ -> onPartial
