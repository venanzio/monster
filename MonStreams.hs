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

-- "lift" monadic actions from the elements to the stream
liftMS :: Monad m => MonStr m (m a) -> m (MonStr m a)
liftMS sm = (\ma ms -> MCons ((,) <$> ma <*> ms))
              <$> (headMS sm) <*> (fmap liftMS (tailMS sm))

-- given a "stream matrix", take the submatrix down one step in the diagonal
diagTail :: Monad m => MonStr m (MonStr m a) -> m (MonStr m (MonStr m a))
diagTail ss = join $ fmap (liftMS . fmap tailMS) (tailMS ss)

joinMS :: Monad m => MonStr m (MonStr m a) -> MonStr m a
joinMS ss = MCons $ (,) <$> join (fmap headMS (headMS ss))
                        <*> fmap joinMS (diagTail ss)

instance Monad m => Monad (MonStr m) where
  -- (>>=) :: MonStr m a -> (a -> MonStr m b) -> MonStr m b
  as >>= f = joinMS (fmap f as)
