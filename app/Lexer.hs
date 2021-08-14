{-# LANGUAGE LambdaCase #-}

module Lexer where

  import qualified Data.Bifunctor as Bifunctor
  infixl 1 |>
  (|>) :: a -> (a -> b) -> b
  x |> f = f x

  infixr 0 <|
  (<|) :: (a -> b) -> a -> b
  (<|) = ($)

  data LexerError = Unexpected Char
    | UnexpectedEOF
    deriving (Show)

  newtype Lexer a = Lexer {
    runLexer :: String -> Either LexerError (a, String)
  }

  instance Show (Lexer a) where
    show (Lexer a) = "Some Lexer"

  instance Functor Lexer where
    fmap f (Lexer g) = Lexer (fmap (Bifunctor.first f) . g)

  instance Applicative Lexer where
    pure a = Lexer (\input -> Right(a, input))
    (Lexer f) <*> (Lexer a) =
      Lexer $ \input -> do
        (f', rest) <- f input
        (a', rest') <- a rest
        return (f' a', rest')

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
