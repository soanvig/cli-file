{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Executor (executeCommand)
  import Command (buildCommand)

  main :: IO ()
  main = do
    contents <- readFileText "commands.cli"
    args <- getArgs

    let command = buildCommand contents args

    case command of
      Left err -> print err
      Right shellCommand -> executeCommand shellCommand
      
      
