module Parser (runParser) where

  import Prelude hiding ((<|>), many)
  import Text.Parsec (oneOf, alphaNum, letter, choice, anyChar, many, many1, manyTill, space, string, char, eof, parse, (<|>), try, sepBy)
  import Text.Parsec.String (Parser)
  import Symbol
  import Data.Bifunctor (first)
  import ParserError


  commandName :: Parser String
  commandName = manyTill anyChar space

  argumentName :: Parser String
  argumentName = many1 alphaNum

  argumentValue :: Parser String
  argumentValue = many1 alphaNum

  argumentRequired :: Parser Argument
  argumentRequired = ArgumentRequired <$> argumentName

  argumentOptional :: Parser Argument
  argumentOptional = ArgumentOptional <$> argumentName <*> (string " = " *> argumentValue)

  arguments :: Parser [Argument]
  arguments = (try argumentOptional <|> argumentRequired) `sepBy` string ", "

  commandQuery :: Parser String
  commandQuery = string "-> " *> manyTill anyChar (() <$ char '\n' <|> eof)

  lineParser :: Parser Command
  lineParser = try withArguments <|> withoutArguments
    where
      withArguments = Command
        <$> commandName
        <* string ":: "
        <*> arguments
        <* string " "
        <*> commandQuery

      withoutArguments = Command 
        <$> commandName
        <*> pure []
        <*> commandQuery

  runParser :: String -> Either String Command
  runParser input = first prettyPrintParseError $ parse lineParser "" input