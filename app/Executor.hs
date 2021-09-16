{-# LANGUAGE OverloadedStrings #-}

module Executor where
  
  import System.Process (shell, createProcess, CreateProcess)
  import Symbol ( Argument(..), Command(..), UserCommand(..) )
  import Control.Exception (throw)
  import Control.Exception.Base (throwIO)
  import Params (replaceCommandParams)

  executeCommand :: String -> IO ()
  executeCommand command = void $ createProcess (shell command)
