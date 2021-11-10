module Helpers where

  import Data.Char (isSpace)

  trim :: String -> String
  trim = f . f
    where f = reverse . dropWhile isSpace

  if' :: Bool -> a -> a -> a
  if' True  x _ = x
  if' False _ y = y