module Main where

  import Lib (executeCommand, buildCommand)

  main :: IO ()
  main = do
    contents <- readFileText "commands.cli"
    args <- getArgs

    let command = buildCommand contents args

    case command of
      Left err -> putStrLn err
      Right shellCommand -> executeCommand shellCommand
      
      
