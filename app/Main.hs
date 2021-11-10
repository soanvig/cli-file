{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Lib (executeCommand, buildCommand)
  import Data.Text (unpack)

  help = unpack $ unlines [
    "cli-file - tool for easy and programming language-agnostic command-line interfaces for executing commands",
    "It offers domain-specific language: configuration file containing `myEcho :: value = World -> echo Hello $value`",
    "can be executed using `cli-file myEcho` resulting in 'Hello World' or `cli-file myEcho John` resulting in 'Hello John'`",
    "",
    "Alpha version: API is subjected to change",
    "For more details see: https://github.com/soanvig/cli-file"
    ]
  version = "0.0.1"

  main :: IO ()
  main = do
    contents <- readFileText "commands.cli"
    args <- getArgs

    case viaNonEmpty head args of
      Just "--help" -> putStrLn help
      Just "-h" -> putStrLn help
      Just "--version" -> putStrLn version
      Just "-v" -> putStrLn version
      _ -> case buildCommand contents args of
        Left err -> putStrLn err
        Right shellCommand -> executeCommand shellCommand
      
      
