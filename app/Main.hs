{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as B

-- Read file from stdin
-- Parse to some common structure
-- Sort!
-- Show / write file

type Modeline = String
type H1 = String
type H2 = String
data Entry = H2 [String] deriving Show
data Wiki = Modeline H1 [Entry] deriving Show

main :: IO ()
main = print "hello sailor"
