{-# LANGUAGE OverloadedStrings #-}

module Executor where
  
  import System.Process (shell, createProcess, CreateProcess)
  import Data.Text (replace, pack, unpack)
  import Symbol ( Argument(..), Command(..), UserCommand(..) )

  type Param = (String, String) -- param name, param value

  matchArgs :: [Argument] -> [String] -> [Param]
  matchArgs [] [] = []
  matchArgs [] (userArg : userArgRest) = undefined
  matchArgs ((ArgumentRequired _) : argRest) [] = undefined
  matchArgs ((ArgumentOptional name value) : argRest) [] = (name, value) : matchArgs argRest []
  matchArgs ((ArgumentRequired name) : argRest) (value : userArgRest) = (name, value) : matchArgs argRest userArgRest
  matchArgs ((ArgumentOptional name _) : argRest) (value : userArgRest) = (name, value) : matchArgs argRest userArgRest

  replaceParams :: String -> [Param] -> String
  replaceParams command [] = command
  replaceParams command (param : paramRest) = replaceParams (unpack replacedCommand) paramRest
    where replacedCommand = replace (pack $ '$' : fst param) (pack $ snd param) (pack command)

  executeCommand :: Command -> UserCommand -> IO ()
  executeCommand (Command _ commandArgs command) (UserCommand _ userCommandArgs) = do
    _ <- createProcess $ shell (replaceParams command $ matchArgs commandArgs userCommandArgs)
    return ()
