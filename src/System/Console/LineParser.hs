module System.Console.LineParser (
  CommandLine (..)
  , fromString
  , toString
  ) where

-- | Algebraic data structure representing the internal format,
-- i.e. parsed, of the command line
data CommandLine = CommandLine String
                 deriving (Show, Eq)

fromString :: String -> CommandLine
fromString s = CommandLine s

toString :: CommandLine -> String
toString (CommandLine s) = s

