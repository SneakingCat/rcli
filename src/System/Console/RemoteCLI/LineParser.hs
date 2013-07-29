module System.Console.RemoteCLI.LineParser (
  CommandLine (..)
  , Scope (..)
  , Option (..)
  , Parameter (..)
  , fromString
  , toString
  ) where

import Text.ParserCombinators.Parsec hiding (option)
import Control.Monad.Writer (Writer, execWriter, tell)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Control.Applicative ((<$>), (<*>), (*>))

type Identifier = String

-- | Algebraic data structure representing the internal format,
-- i.e. parsed, of the command line
data CommandLine = CommandLine Scope Identifier [Option]
                 | ErroneousLine String
                 deriving (Show, Eq)

-- | Tag to tell whether the local or the default scope is requested
data Scope = Local | Default
           deriving (Show, Eq)
                    
-- | Tag to describe an option
data Option = Option Identifier (Maybe Parameter)
            deriving (Show, Eq)
                     
-- | A parameter to an option
data Parameter = Null
               deriving (Show, Eq)

-- | Converts a command line string to the internal format
fromString :: String -> CommandLine
fromString s = 
  case runParser lineParser () "" (stripEnd s) of
    Right commandLine -> commandLine
    Left _            -> ErroneousLine "FEL"

-- | Converts the internal format to a string
toString :: CommandLine -> String
toString cl = execWriter $ serialize cl

-- | Top level parser for the command line
lineParser :: Parser CommandLine
lineParser = CommandLine <$> scope <*> identifier <*> many option

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
    
-- | Parse an options
option :: Parser Option
option = spaces *> (Option <$> identifier <*> optionMaybe parameter)
    
-- | Parse a parameter
parameter :: Parser Parameter
parameter = try (spaces *> char '=' *> spaces *> string "Null" *> return Null)
                    
-- | Serialize the command line to a string
serialize :: CommandLine -> Writer String ()
serialize (CommandLine s c os) = do
  serializeScope s
  serializeId c
  mapM_ serializeOption os
  where
    serializeScope Local = tell ": "
    serializeScope Default = return ()
    serializeId s = tell $ s ++ " "
    serializeOption (Option s p) = do
      serializeId s
      serializeParameter p
    serializeParameter Nothing = return ()
    serializeParameter (Just Null) = tell " = Null "

-- | Strip spaces from the end of the string
stripEnd :: String -> String
stripEnd = dropWhileEnd isSpace