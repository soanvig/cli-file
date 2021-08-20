{-# LANGUAGE LambdaCase #-}

module Parser where

  import Lexer (Token (..))
  import Control.Applicative (Alternative (..))
  import qualified Data.Bifunctor as Bifunctor

  data JsonValue =
    JsonString String
    | JsonNumber Int
    | JsonBool Bool
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]

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
