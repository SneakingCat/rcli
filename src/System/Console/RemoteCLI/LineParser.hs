module System.Console.RemoteCLI.LineParser (
  CommandLine (..)
  , fromString
  , toString
  ) where

import Text.ParserCombinators.Parsec
import Control.Applicative ((<$>), (<*>), (*>))

-- | Algebraic data structure representing the internal format,
-- i.e. parsed, of the command line
data CommandLine = CommandLine String
                 | ErroneousLine String
                 deriving (Show, Eq)

-- | Converts a command line string to the internal format
fromString :: String -> CommandLine
fromString s = 
  case runParser lineParser () "" s of
    Right commandLine -> commandLine
    Left _            -> ErroneousLine "FEL"

-- | Converts the internal format to a string
toString :: CommandLine -> String
toString (CommandLine s) = s

-- | Top level parser for the command line
lineParser :: Parser CommandLine
lineParser = CommandLine <$> identifier

-- | Parse an identifier
identifier :: Parser String
identifier = spaces *> ((:) <$> oneOf beginners <*> many (oneOf followers))
  where
    beginners = ['a'..'z'] ++ ['A'..'Z']
    followers = beginners ++ ['0'..'9'] ++ ['_', '-']

