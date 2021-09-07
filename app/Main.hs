{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Parser
  import Executor
  import Symbol
  import Data.Text (unpack)

  data UserCommand = UserCommand String [String]

  unpackCommands :: [Either ParseError Command] -> Either ParseError [Command]
  unpackCommands = sequence

  userCommand :: [String] -> UserCommand
  userCommand [] = undefined
  userCommand (x : xs) = UserCommand x xs

  matchCommand :: UserCommand -> [Command] -> Command
  matchCommand cmd [] = undefined
  matchCommand (UserCommand userCommand _) (command@(Command cmdName _ _) : xs) | userCommand == cmdName = command
  matchCommand cmd (x : xs) = matchCommand cmd xs

  main :: IO ()
  main = do
    contents <- readFileText "test.cli"
    args <- getArgs

    let commands = map (runParser . unpack) (lines contents)

    case unpackCommands commands of
      Left err -> print err
      Right commands -> executeCommand $ matchCommand (userCommand args) commands
      
