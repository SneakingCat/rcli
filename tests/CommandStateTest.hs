module CommandStateTest where

import CommandLineGen
import CommandStateGen
import Test.QuickCheck
import System.Console.RemoteCLI.CommandState (CommandState
                                              , CommandHandlerEntry
                                              , lookupHandler
                                              , lookupEntry
                                              , empty)
import System.Console.RemoteCLI.CommandLine (CommandLine (..))

-- | Wrapper data type to generate data to entry lookup test case
data ToBeFoundEntry = ToBeFoundEntry String CommandHandlerEntry CommandState
                    deriving Show
                             
instance Arbitrary ToBeFoundEntry where
  arbitrary = do
    entry@(cmd, _) <- handler
    state          <- stateWithDummy entry
    return $ ToBeFoundEntry cmd entry state

-- | When the state is empty, the string "Command "X" not found"
-- always shall be returned when looking up the handle
pCommandNotFoundOnEmpty :: CommandLine -> Bool
pCommandNotFoundOnEmpty commandLine =
  case lookupHandler commandLine empty of
    Left [err] -> err == "Command " ++ show (name commandLine) ++ " not found"
    _          -> False
    where
      name (CommandLine _ n _) = n
      
-- | When the state is empty, the requested command entry shall not be
-- found in none of the scopes
pEntryNotFoundOnEmpty :: String -> Bool
pEntryNotFoundOnEmpty cmd = Nothing == lookupEntry cmd empty

-- | With the pre-populated state the command shall be found and equal
-- to the provided entry
pEntryShallBeFoundAndEqual :: ToBeFoundEntry -> Bool
pEntryShallBeFoundAndEqual (ToBeFoundEntry cmd entry state) =
  case lookupEntry cmd state of
    Just entry' -> entry' == entry
    Nothing     -> False