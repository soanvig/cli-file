{-# LANGUAGE OverloadedStrings #-}

module Executor(executeCommand) where
  
  import System.Process (shell, createProcess)

  executeCommand :: String -> IO ()
  executeCommand command = void $ createProcess (shell command)
