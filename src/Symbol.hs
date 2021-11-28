module Symbol where

    data Argument = ArgumentOptional String String
      | ArgumentRequired String
      deriving (Show)

    data UserCommand = UserCommand String [String]

    data Command = Command String [Argument] String

    data ParserToken = ParserCommand String [Argument] String
      | ParserComment String
      | ParserEmptyLine
      deriving (Show)
