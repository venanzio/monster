module Examples.GenericStreams where

import Prelude hiding (head, tail)
  
import MonadicStreams
import Control.Monad

-- | These are streams that are defined for any monad

-- |  This is used for examples that require zipping elements
-- with their indicies
nats :: Monad m => MonStr m Int
nats = fromN 0
       where fromN n = MCons . return $ (n, fromN (n+1))