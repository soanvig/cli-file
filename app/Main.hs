{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Parser
  import Executor
  import Symbol
  import Data.Text (unpack)
  import Params (replaceCommandParams)
  
  unpackCommands :: [Either ParseError Command] -> Either ParseError [Command]
  unpackCommands = sequence

  makeUserCommand :: [String] -> UserCommand
  makeUserCommand [] = undefined
  makeUserCommand (x : xs) = UserCommand x xs

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
      Right commands -> executeCommand parametrizedCommand
        where userCommand = makeUserCommand args
              parametrizedCommand = replaceCommandParams (matchCommand userCommand commands) userCommand
      
      
