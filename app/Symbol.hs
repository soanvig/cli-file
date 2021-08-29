module Symbol where

    data Argument = ArgumentOptional String String
      | ArgumentRequired String
      deriving (Show)
    
    data Command = Command String Argument String
      deriving (Show)