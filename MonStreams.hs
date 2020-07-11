-- Monadic Streams
--   Venanzio Capretta, 2020

module MonStreams where

import Control.Applicative
import Control.Monad

-- Type of monadic streams
-- f is not required to be a monad

data MonStr m a = MCons (m (a , MonStr m a))

headMS :: Functor m => MonStr m a -> m a
headMS (MCons m) = fmap fst m

tailMS :: Functor m => MonStr m a -> m (MonStr m a)
tailMS (MCons m) = fmap snd m

-- MonStr class instances and helper functions
----------------------------------------------

instance Functor m => Functor (MonStr m) where
  -- fmap :: (a -> b) -> MonStr m a -> MonStr m b
  fmap f (MCons m) = MCons $ fmap (\(a,s) -> (f a, fmap f s)) m

instance Applicative m => Applicative (MonStr m) where
  -- pure :: a -> MonStr m a
  pure a = MCons $ pure (a, pure a)

  -- (<*>) :: MonStr m (a->b) -> MonStr m a -> MonStr m b
  MCons fs <*> MCons as = MCons $
    (\(f,fs') -> \(a,as') -> (f a, fs' <*> as')) <$> fs <*> as

joinMS :: Monad m => MonStr m (MonStr m a) -> MonStr m a
joinMS ss = MCons $ (,) <$> join (fmap headMS (headMS ss))
                        <*> fmap joinMS (join (fmap tailMS (tailMS ss)))

instance Monad m => Monad (MonStr m) where
  -- (>>=) :: MonStr m a -> (a -> MonStr m b) -> MonStr m b
  as >>= f = joinMS (fmap f as)
