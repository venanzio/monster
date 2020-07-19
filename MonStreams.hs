-- Monadic Streams
--   Venanzio Capretta, 2020

module MonStreams where

import Control.Applicative
import Control.Monad

-- Type of monadic streams
-- m is not required to be a monad

data MonStr m a = MCons (m (a , MonStr m a))

-- Terminology: "monster" means monadic stream, an element of: MonStr m a
--  m-monster means an monadic stream under the monad, an element of: m (MonStr m a)

unwrapMS :: MonStr m a -> m (a, MonStr m a)
unwrapMS (MCons m) = m


{- Infix notation for monadic streams:
   We use (<:) for "pure cons": appending a pure element in front of a monster
     (<:) :: a -> MonStr m a -> Monstr m a        -- only for m Applicative

   We add an extra colon when one of the argument is under m:
     (<::) :: m a -> Monstr m a -> Monstr m a     -- for m Functor
     (<:::) :: m a -> m Monstr m a -> Monstr m a  -- for m Applicative
-}


-- MonStr class instances and helper functions
----------------------------------------------

-- Operations for m Functor

headMS :: Functor m => MonStr m a -> m a
headMS = fmap fst . unwrapMS

tailMS :: Functor m => MonStr m a -> m (MonStr m a)
tailMS = fmap snd . unwrapMS

-- Appending an m-element in front of a stream
infixr 5 <::
(<::) :: (Functor m) => m a -> MonStr m a -> MonStr m a
ma <:: s = MCons (fmap (\a -> (a,s)) ma)

instance Functor m => Functor (MonStr m) where
  -- fmap :: (a -> b) -> MonStr m a -> MonStr m b
  fmap f = MCons . fmap (\(a,s) -> (f a, fmap f s)) . unwrapMS


-- Operations for m Applicative

-- Generic pairing operation for Applicatives
pairA :: Applicative m => m a -> m b -> m (a,b)
pairA = liftA2 (,)

-- appending an m-element in front of an m-stream
infix 5 <:::
(<:::) :: Applicative m => m a -> m (MonStr m a) -> MonStr m a
ma <::: ms = MCons (pairA ma ms)

-- Appends a pure element to the beginning of the given monadic stream
infixr 5 <:
(<:) :: (Applicative m) => a -> MonStr m a -> MonStr m a
a <: s = pure a <:: s

-- Double Applicative
infix 5 <<*>>
(<<*>>) :: (Applicative m1, Applicative m2) =>
           m1 (m2 (a->b)) -> m1 (m2 a) -> m1 (m2 b)
(<<*>>) = liftA2 (<*>)

instance Applicative m => Applicative (MonStr m) where
  -- pure :: a -> MonStr m a
  pure a = a <: pure a -- constant stream

  -- (<*>) :: MonStr m (a->b) -> MonStr m a -> MonStr m b
  fs <*> as = (headMS fs <*> headMS as) <::: (tailMS fs <<*>> tailMS as)
    -- (headMS fs <*> headMS as) <:: (tailMS fs <*> tailMS as) 


-- Operations for m Monad




-- "lift" monadic actions from the elements to the stream
liftMS :: Monad m => MonStr m (m a) -> m (MonStr m a)
liftMS sm = do ma <- headMS sm
               a  <- ma
               ms <- tailMS sm
               s <- ms
               return ma <: liftMS s
{-
  (\ma ms -> MCons ((,) <$> ma <*> ms))
              <$> (headMS sm) <*> (fmap liftMS (tailMS sm))
-}
  
-- given a "stream matrix", take the submatrix down one step in the diagonal
diagTail :: Monad m => MonStr m (MonStr m a) -> m (MonStr m (MonStr m a))
diagTail ss = join $ fmap (liftMS . fmap tailMS) (tailMS ss)

joinMS :: Monad m => MonStr m (MonStr m a) -> MonStr m a
joinMS ss = join (fmap headMS (headMS ss)) <::: fmap joinMS (diagTail ss)

{-

  MCons $ (,) <$> join (fmap headMS (headMS ss))
                        <*> fmap joinMS (diagTail ss)
-}

instance Monad m => Monad (MonStr m) where
  -- (>>=) :: MonStr m a -> (a -> MonStr m b) -> MonStr m b
  as >>= f = joinMS (fmap f as)
