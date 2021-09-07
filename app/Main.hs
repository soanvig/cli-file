{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Parser
  import Executor
  import Symbol
  import Data.Text (unpack)

  unpackCommands :: [Either ParseError Command] -> Either ParseError [Command]
  unpackCommands = sequence
  
  main :: IO ()
  main = do
    contents <- readFileText "test.cli"
    args <- getArgs
    let commands = map (runParser . unpack) (lines contents)

    case unpackCommands commands of
      Left err -> print err
      Right commands -> executeCommandList commands
      
