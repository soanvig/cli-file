module ParserError where

  import Text.Parsec (ParseError)
  import Text.Parsec.Error (errorMessages, errorPos, errorMessages, Message(..))
  import Text.Parsec.Pos (sourceColumn, sourceLine)

  prettyPrintParseError :: ParseError -> String
  prettyPrintParseError err = "Error during parsing command file\n"
    -- Because we don't parse whole file at once, but line after line,
    -- this will always be line: 1
    -- ++ "On line: " ++ show errorLine ++ ", "
    ++ "column: " ++ show errorColumn
    ++ joinedMessage
    where
      errorColumn = sourceColumn $ errorPos err
      errorLine = sourceLine $ errorPos err
      messages = map mapError $ errorMessages err
      joinedMessage = concatMap ("\n" ++) messages
      mapError (SysUnExpect msg) = "Unexpected: " ++ msg
      mapError (UnExpect msg) = "Unexpected: " ++ msg
      mapError (Expect msg) = "Expecting: " ++ msg
      mapError (Message msg) = "Message: " ++ msg