{-# LANGUAGE LambdaCase #-}

module Lexer (Token (..), lexer, LexerError) where

  import Control.Applicative (Alternative (..))
  import qualified Data.Bifunctor as Bifunctor
  import GHC.List (foldl1')
  import GHC.Unicode (isDigit)
  import Data.Char (isSpace)
  import qualified Data.Maybe (fromMaybe)

  -- infixl 1 |>
  -- (|>) :: a -> (a -> b) -> b
  -- x |> f = f x

  -- infixr 0 <|
  -- (<|) :: (a -> b) -> a -> b
  -- (<|) = ($)

  data LexerError = Unexpected Char
    | UnexpectedEOF
    deriving (Show)

  data Token =
    StringLiteral String
    | IntLiteral Int
    | BoolLiteral Bool
    | NullLiteral
    | Comma -- ,
    | Colon -- :
    | BracketOpen -- [
    | BracketClose -- ]
    | BraceOpen -- {
    | BraceClose -- }
    | Whitespace
    deriving (Eq, Show)

  newtype Lexer a = Lexer {
    runLexer :: String -> Either LexerError (a, String)
  }

  instance Functor Lexer where
    fmap f (Lexer g) = Lexer (fmap (Bifunctor.first f) . g)

  instance Applicative Lexer where
    pure a = Lexer (\input -> Right(a, input))
    (Lexer f) <*> (Lexer a) =
      Lexer $ \input -> do
        (f', rest) <- f input
        (a', rest') <- a rest
        return (f' a', rest')

  instance Alternative Lexer where
    empty = Lexer (Left . unexpected)
    (Lexer lA) <|> (Lexer lB) =
      Lexer $ \input -> case (lA input, lB input) of
        (res, Left _) -> res
        (Left _, res) -> res
        (a@(Right (_, restA)), b@(Right (_, restB))) ->
          if length restA <= length restB then a else b

  unexpected :: String -> LexerError
  unexpected [] = UnexpectedEOF
  unexpected (c : _) = Unexpected c

  expects :: (Char -> Bool) -> Lexer Char
  expects predicate =
    Lexer $ \case
      char : rest | predicate char -> Right (char, rest)
      rest -> Left $ unexpected rest

  char :: Char -> Lexer Char
  char c = expects (c ==)

  digit :: Lexer Char
  digit = expects isDigit

  string :: String -> Lexer String
  string = traverse char

  number :: Lexer Int
  number = maybeReadNumber <$> some digit
    where
      maybeReadNumber :: String -> Int
      maybeReadNumber str = fromMaybe 0 (readMaybe str) -- 0 case should never happen in fact

  whitespace :: Lexer String
  whitespace = some $ expects isSpace

  oneOf :: [Lexer a] -> Lexer a
  oneOf = foldl1' (<|>)

  token :: Lexer Token
  token = operator <|> literal
    where
      operator = oneOf
        [
          Comma <$ string ",",
          Colon <$ string ":",
          BracketOpen <$ string "[",
          BracketClose <$ string "]",
          BraceOpen <$ string "{",
          BraceClose <$ string "}",
          Whitespace <$ whitespace
        ]
      literal = stringLiteral <|> intLiteral <|> boolLiteral <|> nullLiteral
        where
          stringLiteral = StringLiteral <$> (char '"' *> many (expects (/= '"')) <* char '"')
          intLiteral = IntLiteral <$> number
          boolLiteral = (BoolLiteral True <$ string "true") <|> (BoolLiteral False <$ string "false")
          nullLiteral = NullLiteral <$ string "null"

  removeWhitespaces :: [Token] -> [Token]
  removeWhitespaces = filter (/= Whitespace)

  lexer :: String -> Either LexerError ([Token], String)
  lexer = fmap filtering <$> runLexer (some token)
    where filtering = Bifunctor.first removeWhitespaces