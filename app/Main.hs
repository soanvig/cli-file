module Main where

  import Lexer
  import Parser
  import qualified Data.Bifunctor as Bifunctor

  main :: IO ()
  main = putStrLn "Hello, Haskell!"

  run :: String -> Either Lexer.LexerError [Token]
  run input = do
    (tokens, rest) <- lexer input
    return tokens

  test :: Either Lexer.LexerError [Token] -> Either ParserError (JsonValue, [Token])
  test (Right tokens) = parser tokens
  test (Left _) = undefined
    