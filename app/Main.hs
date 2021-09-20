{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Parser
  import Executor
  import Symbol
  import Data.Text (unpack)
  import Params (replaceCommandParams)
  import Data.Bifunctor (first)
  import Text.Parsec.Error (Message (Message) ,errorMessages , messageString)
  
  unpackCommands :: [Either ParseError Command] -> Either ParseError [Command]
  unpackCommands = sequence

  makeUserCommand :: [String] -> Either String UserCommand
  makeUserCommand [] = Left "No command given"
  makeUserCommand (x : xs) = Right $ UserCommand x xs

  matchCommand :: UserCommand -> [Command] -> Either String Command
  matchCommand cmd [] = Left "No matching command found"
  matchCommand (UserCommand userCommand _) (command@(Command cmdName _ _) : xs) | userCommand == cmdName = Right command
  matchCommand cmd (x : xs) = matchCommand cmd xs

  prettyPrintParseError :: ParseError -> String
  prettyPrintParseError = intercalate "," . map messageString . errorMessages

  buildCommand :: Text -> [String] -> Either String String
  buildCommand commandFile args = do
    commands <- first prettyPrintParseError $ unpackCommands $ map (runParser . unpack) (lines commandFile)
    userCommand <- makeUserCommand args
    matchedCommand <- matchCommand userCommand commands
    replaceCommandParams matchedCommand userCommand

  main :: IO ()
  main = do
    contents <- readFileText "test.cli"
    args <- getArgs

    let command = buildCommand contents args

    case command of
      Left err -> print err
      Right shellCommand -> executeCommand shellCommand
      
      
