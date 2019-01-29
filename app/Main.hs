{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude
import qualified Data.ByteString.Char8 as B
import Control.Applicative ((<|>))
import Text.Trifecta
import Text.RawString.QQ

fp :: FilePath
fp = "/home/decoy/Documents/vimwiki/study/category-theory/terminology.md"

newtype Modeline = Modeline String deriving (Show)
newtype H1 = H1 String deriving (Show)
newtype H2 = H2 String deriving (Show)
newtype Line = Line String deriving (Show)
data Entry = Entry H2 String
data Wiki = Wiki Modeline H1 [Entry]

data LineOrH2 = IsLine Line | IsH2 H2

instance Show Entry where
  show (Entry (H2 h2) lines) = "## " <> h2 <> "\n" <> lines

instance Show Wiki where
  show (Wiki (Modeline modeline) (H1 h1) entries) = all where
    all = modeline <> "\n\n" <>
          "# " <> h1 <> "\n\n" <>
          foldr (\e acc -> show e <> acc) mempty entries
          <> "Number of entries:" <> show (length entries)

--------------------------------------------------------------------------------

isNewLine :: Char -> Bool
isNewLine = (==) '\n'

skipNewlines :: Parser ()
skipNewlines = skipMany newline

tilEol :: Parser String
tilEol = many (notChar '\n')

parseModeline :: Parser Modeline
parseModeline = do
  x <- tilEol
  return (Modeline x)

parseH1 :: Parser H1
parseH1 = do
  string "# "
  h1 <- tilEol
  return (H1 h1)

parseH2 :: Parser H2
parseH2 = do
  string "## "
  h2 <- tilEol
  return (H2 h2)

parseEntry :: Parser Entry
parseEntry = do
  h2 <- parseH2
  newline
  lines <- many (notFollowedBy parseH2 >> anyChar)
  return $ Entry h2 lines

-- Think, what do we want?
-- We want to capture `many anyChar` lines,
-- but stop caturing after arriving at an h2 line

parseWiki :: Parser Wiki
parseWiki = do
  modeline <- parseModeline
  skipNewlines
  h1 <- parseH1
  entries <- many (skipNewlines >> parseEntry)
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
