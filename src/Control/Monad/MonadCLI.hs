{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.MonadCLI (
  CLI
  , runCLI
  , get
  , put
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)

-- | The monadic CLI type. Is wrapping StateT with the IO monad at the
-- bottom
newtype CLI s a = CLI {
  extractCLI :: StateT s IO a
  } deriving (Monad, MonadState s, MonadIO)
             
-- | Run the CLI monad, leaving only the IO a part left
runCLI :: CLI s a -> s -> IO a
runCLI m = evalStateT (extractCLI m)