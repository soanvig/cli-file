{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Parser
  import Executor
  import Symbol
  import Data.Text (unpack)

  unpackCommands :: [Either ParseError Command] -> Either ParseError [Command]
  unpackCommands = sequence

  userCommand :: [String] -> String
  userCommand [] = undefined
  userCommand (x : xs) = x

  matchCommand :: String -> [Command] -> Command
  matchCommand cmd [] = undefined
  matchCommand cmd (command@(Command cmdName _ _) : xs) | cmd == cmdName = command
  matchCommand cmd (x : xs) = matchCommand cmd xs

  main :: IO ()
  main = do
    contents <- readFileText "test.cli"
    args <- getArgs

    let commands = map (runParser . unpack) (lines contents)

    case unpackCommands commands of
      Left err -> print err
      Right commands -> executeCommand $ matchCommand (userCommand args) commands
      
