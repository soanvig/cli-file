module Executor where
  
  import System.Process (shell, createProcess, CreateProcess)
  import Symbol

  executeCommand :: Command -> IO ()
  executeCommand (Command _ _ command) = do
    _ <- createProcess $ shell command
    return ()

  executeCommandList :: [Command] -> IO ()
  executeCommandList [] = return ()
  executeCommandList (command : rest) = do
    executeCommand command
    executeCommandList rest
