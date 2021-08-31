{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Parser
  import Data.Text (unpack)

  main :: IO ()
  main = do
    contents <- readFileText "test.cli"
    print $ map (runParser . unpack) (lines contents)
