module Params (replaceCommandParams) where

  import Data.Text (replace, pack, unpack)
  import Symbol ( Argument(..), Command(..), UserCommand(..) )
  
  type Param = (String, String) -- param name, param value

  joinEitherList :: t -> Either String [t] -> Either String [t]
  joinEitherList arr (Left error) = Left error
  joinEitherList arr (Right value) = Right (arr : value)

  matchArgs :: [Argument] -> [String] -> Either String [Param]
  matchArgs [] (userArg : userArgRest) = Left "Supplied more parameters than expected"
  matchArgs ((ArgumentRequired _) : argRest) [] = Left "Supplied less parameters than expected"
  matchArgs [] [] = Right []
  matchArgs ((ArgumentOptional name value) : argRest) [] =
    joinEitherList (name, value) (matchArgs argRest [])
  matchArgs ((ArgumentRequired name) : argRest) (value : userArgRest) =
    joinEitherList (name, value) (matchArgs argRest userArgRest)
  matchArgs ((ArgumentOptional name _) : argRest) (value : userArgRest) =
    joinEitherList (name, value) (matchArgs argRest userArgRest)

  replaceParams :: String -> [Param] -> String
  replaceParams command [] = command
  replaceParams command (param : paramRest) = replaceParams (toString replacedCommand) paramRest
    where replacedCommand = replace (toText $ '$' : fst param) (toText $ snd param) (toText command)

  replaceCommandParams :: Command -> UserCommand -> Either String String
  replaceCommandParams (Command _ commandArgs command) (UserCommand _ userCommandArgs) =
    replaceParams command <$> matchArgs commandArgs userCommandArgs