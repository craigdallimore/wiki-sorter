{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude
import qualified Data.ByteString.Char8 as B
import Text.Trifecta
import Text.Trifecta.Combinators (lookAhead)

fp :: FilePath
fp = "/home/decoy/Documents/vimwiki/study/category-theory/terminology.md"

type Modeline = String
type H1 = String
type H2 = String
data Entry = Entry H2 String
data Wiki = Wiki Modeline H1 [Entry]

instance Show Entry where
  show (Entry h2 lines) = "## " <> h2 <> "\n" <> lines

instance Show Wiki where
  show (Wiki modeline h1 entries) = all where
    all = modeline <> "\n\n" <>
          "# " <> h1 <> "\n\n" <>
          foldr (\e acc -> show e <> acc) mempty entries
          <> show (length entries)

isNewLine :: Char -> Bool
isNewLine = (==) '\n'

--------------------------------------------------------------------------------

skipSpace :: Parser ()
skipSpace = skipMany (oneOf "\n ")

parseModeline :: Parser Modeline
parseModeline = do
  x <- manyTill anyChar (string "\n")
  return x

parseH1 :: Parser H1
parseH1 = string "# " >> manyTill anyChar newline

parseH2 :: Parser H2
parseH2 = string "## " >> manyTill anyChar newline

parseLines :: Parser String
parseLines = manyTill anyChar (lookAhead parseH2)

newtype H2orOther = H2 | Other

parseEntries :: Parser [Entry]
parseEntries = do
  content <- (H2 <$> try parseH2) <|> (Other <$> manyTill anyChar newline)

  Entry <$> parseH2 <*> parseLines

parseWiki :: Parser Wiki
parseWiki = do
  modeline <- parseModeline
  skipSpace
  h1 <- parseH1
  skipSpace
  -- entries <- many parseEntry
  entries <- parseEntries
  return (Wiki modeline h1 entries)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Please provide a filename"
  name <- getLine
  file <- B.readFile fp
  let res = parseString parseWiki mempty (B.unpack file)
  case res of
    Success wiki -> putStrLn (show wiki)
    Failure err -> print (_errDoc err)
