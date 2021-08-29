module Parser where

  import Prelude hiding ((<|>))
  import Text.Parsec (anyChar, manyTill, space, string, char, eof, parse, (<|>), try)
  import Text.Parsec.String (Parser)
  import Symbol

  commandNameParser :: Parser String
  commandNameParser = manyTill anyChar space

  argumentRequiredParser :: Parser Argument
  argumentRequiredParser = ArgumentRequired <$> (string ":: " *> manyTill anyChar space)

  argumentOptionalParser :: Parser Argument
  argumentOptionalParser = ArgumentOptional <$> (string ":: " *> manyTill anyChar space) <*> (string "= " *> manyTill anyChar space)

  commandQueryParser :: Parser String
  commandQueryParser = string "-> " *> manyTill anyChar (() <$ char '\n' <|> eof)

  lineParser :: Parser Command
  lineParser = Command <$> commandNameParser <*> (try argumentOptionalParser <|> argumentRequiredParser) <*> commandQueryParser

  runParser = parse lineParser ""