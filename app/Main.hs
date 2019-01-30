{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Applicative ((<|>))
import Text.Trifecta
import Text.RawString.QQ
import Data.List (sort)
import qualified Data.Text as T
import System.IO
import System.Environment

newtype Modeline = Modeline String deriving (Show)
newtype H1 = H1 String deriving (Show)
newtype H2 = H2 String deriving (Show, Eq)
data Entry = Entry H2 String deriving (Eq)
data Wiki = Wiki Modeline H1 [Entry]

instance Ord Entry where
  compare (Entry (H2 a) _) (Entry (H2 b) _) = compare a b

instance Show Entry where
  show (Entry (H2 h2) lines) = "## " <> h2 <> "\n" <> lines

instance Show Wiki where
  show (Wiki (Modeline modeline) (H1 h1) entries) = all where
    all = modeline <> "\n\n" <>
          "# " <> h1 <>
          foldr (\e acc -> "\n\n" <> show e <> acc) mempty entries

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

processEntries :: [Entry] -> [Entry]
processEntries = sort . map stripline
  where
    stripline (Entry h2 lines) = Entry h2 (strip' lines)
    strip' = T.unpack . T.strip . T.pack

parseWiki :: Parser Wiki
parseWiki = do
  modeline <- parseModeline
  skipNewlines
  h1 <- parseH1
  entries <- many (skipNewlines >> parseEntry)
  return (Wiki modeline h1 (processEntries entries))

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      inputHandle <- openFile filename ReadMode
      hSetEncoding inputHandle utf8
      fileContent <- hGetContents inputHandle
      let res = parseString parseWiki mempty fileContent
      case res of
        Success wiki -> do
          outputHandle <- openFile filename WriteMode
          hSetEncoding outputHandle utf8
          hPutStr outputHandle (show wiki)
          hClose inputHandle
          hClose outputHandle
        Failure err -> print (_errDoc err)
    [] -> do
      putStrLn "No file specified"
