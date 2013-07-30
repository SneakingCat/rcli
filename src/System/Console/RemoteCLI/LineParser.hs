module System.Console.RemoteCLI.LineParser (
  CommandLine (..)
  , Scope (..)
  , Option (..)
  , Value (..)
  , fromString
  , toString
  ) where

import Text.ParserCombinators.Parsec
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
data Option = Option Identifier (Maybe Value)
            deriving (Show, Eq)
                     
-- | A value to an option
data Value = Null
           | Bool Bool
           | Int Int
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
lineParser = CommandLine <$> scope <*> identifier <*> many anOption

-- | Parse the scope
scope :: Parser Scope
scope = spaces *> (option Default $ (char ':' *> return Local))

-- | Parse an identifier
identifier :: Parser String
identifier = spaces *> ((:) <$> oneOf beginners <*> many (oneOf followers))
  where
    beginners = ['a'..'z'] ++ ['A'..'Z']
    followers = beginners ++ ['0'..'9'] ++ ['_', '-']
    
-- | Parse an option
anOption :: Parser Option
anOption = spaces *> (Option <$> identifier <*> value)
    
-- | Parse a value
value :: Parser (Maybe Value)
value = spaces *> ((char '=' *> determineValue)
                   <|> return Nothing)
  where
    determineValue = try valueNull
                     <|> try valueBool
                     <|> try valueInt
                     <?> "A valid type"

-- | Parsing a Null literal
valueNull :: Parser (Maybe Value)
valueNull = spaces *> string "Null" *> return (Just Null)

-- | Parsing a bool literal
valueBool :: Parser (Maybe Value)
valueBool = spaces *> 
            (Just <$> (Bool . read) <$> (string "True" <|> string "False"))
            
-- | Parsing an int literal
valueInt :: Parser (Maybe Value)
valueInt = spaces *> (Just <$> (Int . read) <$> num)
  where
    num    = (:) <$> sign <*> digits
    sign   = option ' ' (char '-')
    digits = many1 digit

-- | Serialize the command line to a string
serialize :: CommandLine -> Writer String ()
serialize (CommandLine s c os) = do
  serializeScope s
  serializeIdentity c
  mapM_ serializeOption os
  where
    serializeScope Local   = tell ":" >> blank
    serializeScope Default = return ()
    serializeIdentity i    = tell i >> blank
    serializeOption (Option i p) = do
      serializeIdentity i
      serializeParameter p
    serializeParameter Nothing         = return ()
    serializeParameter (Just Null)     = tell "= Null" >> blank
    serializeParameter (Just (Bool b)) = assign b >> blank
    serializeParameter (Just (Int n))  = assign n >> blank
    assign x = tell $ "= " ++ show x
    blank = tell " "

-- | Strip spaces from the end of the string
stripEnd :: String -> String
stripEnd = dropWhileEnd isSpace