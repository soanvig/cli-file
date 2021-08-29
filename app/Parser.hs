module Parser where

  import Text.Parsec ( anyChar, manyTill, space, string, char, eof )
  import Text.Parsec.String (Parser)
  
  data Command = Command String [String] String
    deriving (Show)

  commandNameParser :: Parser String
  commandNameParser = manyTill anyChar space

  argumentParser :: Parser [String]
  argumentParser = (: []) <$> (string ":: " *> manyTill anyChar space)

  commandQueryParser :: Parser String
  commandQueryParser = string "-> " *> manyTill anyChar (() <$ char '\n' <|> eof)

  lineParser :: Parser Command
  lineParser = Command <$> commandNameParser <*> argumentParser <*> commandQueryParser