module Parser (ParseError, runParser, argumentParser, argumentOptionalParser) where

  import Prelude hiding ((<|>), many)
  import Text.Parsec (oneOf, alphaNum, letter, choice, anyChar, many, many1, manyTill, space, string, char, eof, parse, (<|>), try, sepBy, ParseError)
  import Text.Parsec.String (Parser)
  import Symbol

  commandNameParser :: Parser String
  commandNameParser = manyTill anyChar space

  argumentNameParser :: Parser String
  argumentNameParser = many1 alphaNum

  argumentValueParser :: Parser String
  argumentValueParser = many1 alphaNum

  argumentRequiredParser :: Parser Argument
  argumentRequiredParser = ArgumentRequired <$> argumentNameParser

  argumentOptionalParser :: Parser Argument
  argumentOptionalParser = ArgumentOptional <$> argumentNameParser <*> (string " = " *> argumentValueParser)

  argumentParser :: Parser [Argument]
  argumentParser = (try argumentOptionalParser <|> argumentRequiredParser) `sepBy` string ", "

  commandQueryParser :: Parser String
  commandQueryParser = string " -> " *> manyTill anyChar (() <$ char '\n' <|> eof)

  lineParser :: Parser Command
  lineParser = Command <$> commandNameParser <* string ":: " <*> argumentParser <*> commandQueryParser

  runParser :: String -> Either ParseError Command
  runParser = parse lineParser ""