module Params (replaceCommandParams) where

  import Data.Text (replace, pack, unpack)
  import Symbol
  
  type Param = (String, String) -- param name, param value

  joinEitherList :: t -> Either String [t] -> Either String [t]
  joinEitherList arr (Left error) = Left error
  joinEitherList arr (Right value) = Right (arr : value)

  matchArgs :: [Argument] -> [String] -> Either String [Param]
  -- no expected arguments left
  matchArgs [] (userArg : userArgRest) =
    Left ("Supplied more parameters than expected. Unexpected value: " ++ userArg)
  -- no supplied arguments left
  matchArgs ((ArgumentRequired name) : argRest) [] =
    Left ("Supplied less parameters than expected. Expected argument: $" ++ name)
  -- nothing's left
  matchArgs [] [] =
    Right []
  -- optional argument with default value matched
  matchArgs ((ArgumentOptional name value) : argRest) [] =
    joinEitherList (name, value) (matchArgs argRest [])
  -- optional argument matched
  matchArgs ((ArgumentOptional name _) : argRest) (value : userArgRest) =
    joinEitherList (name, value) (matchArgs argRest userArgRest)
  -- required argument matched
  matchArgs ((ArgumentRequired name) : argRest) (value : userArgRest) =
    joinEitherList (name, value) (matchArgs argRest userArgRest)

  -- Replace arguments in command string with matching params
  replaceParams :: String -> [Param] -> String
  replaceParams command [] = command
  replaceParams command (param : paramRest) = replaceParams (toString replacedCommand) paramRest
    where
      expectedParam = toText $ '$' : fst param
      value = toText $ snd param
      replacedCommand = replace expectedParam value (toText command)

  -- Replace params in Command with params supplied in UserCommand
  replaceCommandParams :: Command -> UserCommand -> Either String String
  replaceCommandParams (Command _ commandArgs command) (UserCommand _ userCommandArgs) =
    replaceParams command <$> matchArgs commandArgs userCommandArgs
