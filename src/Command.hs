{-# LANGUAGE OverloadedStrings #-}

module Command (buildCommand) where

  import Symbol
  import Data.Text (unpack)
  import Params (replaceCommandParams)
  import Parser
  import Helpers

  makeUserCommand :: [String] -> Either String UserCommand
  makeUserCommand [] = Left "No command given"
  makeUserCommand (x : xs) = Right $ UserCommand x xs

  matchCommand :: UserCommand -> [Command] -> Either String Command
  matchCommand cmd [] = Left "No matching command found"
  matchCommand (UserCommand userCommand _) (command@(Command cmdName _ _) : xs) | userCommand == cmdName = Right command
  matchCommand cmd (x : xs) = matchCommand cmd xs

  filterCommands :: [ParserToken] -> [Command]
  filterCommands [] = []
  filterCommands ((ParserComment _) : rest) = filterCommands rest
  filterCommands ((ParserCommand a b c) : rest) = Command a b c : filterCommands rest
  filterCommands (ParserEmptyLine : rest) = filterCommands rest

  buildCommand :: Text -> [String] -> Either String String
  buildCommand commandFile args = do
    parserTokens <- runParser (unpack commandFile)
    let commands = filterCommands parserTokens
    userCommand <- makeUserCommand args
    matchedCommand <- matchCommand userCommand commands
    replaceCommandParams matchedCommand userCommand
