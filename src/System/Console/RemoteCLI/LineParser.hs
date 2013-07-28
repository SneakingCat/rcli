module System.Console.RemoteCLI.LineParser (
  CommandLine (..)
  , Scope (..)
  , fromString
  , toString
  ) where

import Text.ParserCombinators.Parsec
import Control.Monad.Writer
import Control.Applicative ((<$>), (<*>), (*>))

-- | Algebraic data structure representing the internal format,
-- i.e. parsed, of the command line
data CommandLine = CommandLine Scope String
                 | ErroneousLine String
                 deriving (Show, Eq)

-- | Tag to tell whether the local or the default scope is requested
data Scope = Local | Default
           deriving (Show, Eq)

-- | Converts a command line string to the internal format
fromString :: String -> CommandLine
fromString s = 
  case runParser lineParser () "" s of
    Right commandLine -> commandLine
    Left _            -> ErroneousLine "FEL"

-- | Converts the internal format to a string
toString :: CommandLine -> String
toString cl = execWriter $ serialize cl

-- | Top level parser for the command line
lineParser :: Parser CommandLine
lineParser = CommandLine <$> scope <*> identifier

-- | Parse the scope
scope :: Parser Scope
scope = spaces *> (try (char ':') *> return Local
                   <|> return Default)

-- | Parse an identifier
identifier :: Parser String
identifier = spaces *> ((:) <$> oneOf beginners <*> many (oneOf followers))
  where
    beginners = ['a'..'z'] ++ ['A'..'Z']
    followers = beginners ++ ['0'..'9'] ++ ['_', '-']
    
-- | Serialize the command line to a string
serialize :: CommandLine -> Writer String ()
serialize (CommandLine s c) = do
  serializeScope s
  serializeCmd c
  where
    serializeScope Local = tell ": "
    serializeScope Default = return ()
    serializeCmd s = tell $ s ++ " "

