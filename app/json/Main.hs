{-# LANGUAGE OverloadedStrings #-}

module Main where

  import Lexer ( lexer, Token, LexerError )
  import Parser
  import qualified Data.Bifunctor as Bifunctor

  main = putStr "Welcome" 

  run :: String -> Either ParserError JsonValue
  run input = runParser $ fst <$> lexer input

  runParser :: Either Lexer.LexerError [Token] -> Either ParserError JsonValue
  runParser (Right tokens) = parser tokens
  runParser (Left _) = error "cannot parse json"
    