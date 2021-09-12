module Executor where
  
  import System.Process (shell, createProcess, CreateProcess)
  import Symbol

  replaceArgs :: Command -> UserCommand -> Command
  replaceArgs _ _ = Command "" [] ""

  executeCommand :: Command -> UserCommand -> IO ()
  executeCommand (Command _ commandArgs command) (UserCommand _ userCommandArgs) = do
    _ <- createProcess $ shell command
    return ()
