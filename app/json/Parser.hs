{-# LANGUAGE LambdaCase #-}

module Parser (JsonValue (..), parser, ParserError) where

  import Lexer (Token (..))
  import Control.Applicative (Alternative (..), liftA2)
  import qualified Data.Bifunctor as Bifunctor

  data JsonValue =
    JsonString String
    | JsonNumber Int
    | JsonBool Bool
    | JsonNull
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show)

  data ParserError =
    Unexpected Token
    | UnexpectedEOF
    deriving (Show)

  newtype Parser a = Parser {
    runParser :: [Token] -> Either ParserError (a, [Token])
  }

  instance Functor Parser where
    fmap f (Parser g) =  Parser (fmap (Bifunctor.first f) . g)

  instance Applicative Parser where
    pure a = Parser (\input -> Right(a, input))
    (Parser f) <*> (Parser a) =
      Parser $ \input -> do
        (f', rest) <- f input
        (a', rest') <- a rest
        return (f' a', rest')

  instance Alternative Parser where
    empty = Parser (Left . unexpected)
    (Parser lA) <|> (Parser lB) =
      Parser $ \input -> case (lA input, lB input) of
        (res, Left _) -> res
        (Left _, res) -> res
        (a@(Right (_, restA)), b@(Right (_, restB))) ->
          if length restA <= length restB then a else b

  unexpected :: [Token] -> ParserError
  unexpected [] = UnexpectedEOF
  unexpected (c : _) = Unexpected c

  stringParser :: Parser JsonValue
  stringParser = Parser $ \case
    (StringLiteral a) : rest -> Right (JsonString a, rest)
    rest -> Left $ unexpected rest

  numberParser :: Parser JsonValue
  numberParser = Parser $ \case
    (IntLiteral a) : rest -> Right (JsonNumber a, rest)
    rest -> Left $ unexpected rest

  boolParser :: Parser JsonValue
  boolParser = Parser $ \case
    (BoolLiteral a) : rest -> Right (JsonBool a, rest)
    rest -> Left $ unexpected rest

  tokenParser :: Token -> Parser Token
  tokenParser predicate = Parser $ \case
      token : rest | (token == predicate) -> Right (token, rest)
      rest -> Left $ unexpected rest

  manyWithSeparator :: Parser b -> Parser a -> Parser [b]
  manyWithSeparator element sep = (:) <$> element <*> many (sep *> element) <|> pure []
  
  arrayParser :: Parser JsonValue
  arrayParser = JsonArray <$> (tokenParser BracketOpen *> manyWithSeparator jsonValue (tokenParser Comma) <* tokenParser BracketClose)

  objectParser :: Parser JsonValue
  objectParser = JsonObject <$> (tokenParser BraceOpen *> manyWithSeparator declarationParser (tokenParser Comma) <* tokenParser BraceClose)
    where declarationParser = (,) <$> (keyParser <* tokenParser Colon) <*> jsonValue
          keyParser = Parser $ \case
            (StringLiteral a) : rest -> Right (a, rest)
            rest -> Left $ unexpected rest
  
  nullParser :: Parser JsonValue
  nullParser = JsonNull <$ tokenParser NullLiteral

  jsonValue :: Parser JsonValue
  jsonValue = objectParser <|> stringParser <|> numberParser <|> boolParser <|> arrayParser <|> nullParser

  parser :: [Token] -> Either ParserError JsonValue
  parser tokens = case runParser jsonValue tokens of
    Left error -> Left error
    Right (value, []) -> Right value
    Right (_, rest) -> Left $ unexpected rest