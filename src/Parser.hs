module Parser where

  import Prelude hiding ((<|>), many, optional)
  import Text.Parsec (oneOf, alphaNum, letter, choice, anyChar, many, many1, manyTill, space, string, char, eof, parse, (<|>), try, sepBy, sepEndBy, lookAhead, optional, sepEndBy, endOfLine)
  import Text.Parsec.String (Parser)
  import Symbol
  import Data.Bifunctor (first)
  import ParserError

  maybeEol :: Parser ()
  maybeEol = lookAhead $ choice [eof, () <$ endOfLine]

  commandName :: Parser String
  commandName = manyTill anyChar space

  argumentName :: Parser String
  argumentName = many1 alphaNum

  argumentValue :: Parser String
  argumentValue = char '"' *> manyTill anyChar (char '"')

  argumentRequired :: Parser Argument
  argumentRequired = ArgumentRequired <$> argumentName

  argumentOptional :: Parser Argument
  argumentOptional = ArgumentOptional <$> argumentName <*> (string " = " *> argumentValue)

  arguments :: Parser [Argument]
  arguments = choice [try argumentOptional, argumentRequired] `sepBy` string ", "

  commandQuery :: Parser String
  commandQuery = string "-> " *> manyTill anyChar maybeEol

  command :: Parser ParserToken
  command = try withArguments <|> withoutArguments
    where
      withArguments = ParserCommand
        <$> commandName
        <* string ":: "
        <*> arguments
        <* string " "
        <*> commandQuery

      withoutArguments = ParserCommand 
        <$> commandName
        <*> pure []
        <*> commandQuery

  comment :: Parser ParserToken
  comment = ParserComment <$> (char '#' *> manyTill anyChar maybeEol)

  fileParser :: Parser [ParserToken]
  fileParser = many endOfLine *> choice [comment, command] `sepEndBy` many endOfLine <* eof

  runParser :: String -> Either String [ParserToken]
  runParser input = first prettyPrintParseError $ parse fileParser "" input
