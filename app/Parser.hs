module Parser (ParseError, runParser) where

  import Prelude hiding ((<|>))
  import Text.Parsec (anyChar, manyTill, space, string, char, eof, parse, (<|>), try, ParseError)
  import Text.Parsec.String (Parser)
  import Symbol

  allButSpace :: Parser String
  allButSpace = manyTill anyChar space

  commandNameParser :: Parser String
  commandNameParser = allButSpace

  argumentRequiredParser :: Parser Argument
  argumentRequiredParser = ArgumentRequired <$> (string ":: " *> allButSpace)

  argumentOptionalParser :: Parser Argument
  argumentOptionalParser = ArgumentOptional <$> (string ":: " *> allButSpace) <*> (string "= " *> allButSpace)

  commandQueryParser :: Parser String
  commandQueryParser = string "-> " *> manyTill anyChar (() <$ char '\n' <|> eof)

  lineParser :: Parser Command
  lineParser = Command <$> commandNameParser <*> (try argumentOptionalParser <|> argumentRequiredParser) <*> commandQueryParser

  runParser :: String -> Either ParseError Command
  runParser = parse lineParser ""