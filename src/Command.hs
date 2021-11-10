module Command (buildCommand) where

  import Symbol
  import Data.Text (unpack)
  import Params (replaceCommandParams)
  import Parser
  
  unpackCommands :: [Either String Command] -> Either String [Command]
  unpackCommands = sequence

  makeUserCommand :: [String] -> Either String UserCommand
  makeUserCommand [] = Left "No command given"
  makeUserCommand (x : xs) = Right $ UserCommand x xs

  matchCommand :: UserCommand -> [Command] -> Either String Command
  matchCommand cmd [] = Left "No matching command found"
  matchCommand (UserCommand userCommand _) (command@(Command cmdName _ _) : xs) | userCommand == cmdName = Right command
  matchCommand cmd (x : xs) = matchCommand cmd xs

  buildCommand :: Text -> [String] -> Either String String
  buildCommand commandFile args = do
    commands <- unpackCommands $ map (runParser . unpack) (lines commandFile)
    userCommand <- makeUserCommand args
    matchedCommand <- matchCommand userCommand commands
    replaceCommandParams matchedCommand userCommand