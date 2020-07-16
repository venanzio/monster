-- MonStr class instances and helper functions
----------------------------------------------

module MonadStream 
   (
      -- type of Monad streams
      MonStr(..)
      -- basic functions
      , (<:>)
      , (<&>)
      , head
      , tail
      , inits
      --, tails
      , map
      , take
      , drop
   ) 
   where
    
import Prelude hiding (head, tail, map, take, drop)

import Control.Monad
import Control.Monad.Identity
import Stream hiding ((<:>), head, tail, map, take, drop, inits, tails)

data MonStr m a = MCons (m (a , MonStr m a))

instance (Monad m) => Functor (MonStr m) where
  fmap = map

-- Appends a pure element to the beginning of the given monadic stream
infixr 5 <:>
(<:>) :: (Monad m) => a -> MonStr m a -> MonStr m a
a <:> ms = MCons $ return (a, ms)

-- Appends a monadic value to the beginning of the given monadic stream
infixr 5 <&>
(<&>) :: (Monad m) => m a -> MonStr m a -> MonStr m a
ma <&> ms = MCons $ do 
               a <- ma
               return (a, ms)

-- Extracts the first element of the stream, wrapped in the container type
head :: (Monad m) => MonStr m a -> m a
head (MCons ms) = fmap fst ms

-- Extracts the tail of the stream, passing on the monadic action of the first element to the tail stream
tail :: (Monad m) => MonStr m a -> MonStr m a
tail (MCons ms) = MCons $ join $ unwrap <$> snd <$> ms
                        where unwrap (MCons ms') = ms'
   
-- Version of inits which returns a monadic stream        
-- inits :: (Monad m) => MonStr m a -> MonStr m ([m a])
-- inits ms = MCons $ return ([], fmap (head ms :) (inits (tail ms)))

-- Returns a Stream of all possible initial portions of the given MonStr
inits :: (Monad m) => MonStr m a -> Stream ([m a])
inits ms = Cons [] (fmap (head ms :) (inits (tail ms)))

-- Returns a Stream of all possible tails of
tails :: (Monad m) => MonStr m a -> Stream (MonStr m a)
tails ms = Cons ms (tails (tail ms))

map :: (Monad m) => (a -> b) -> MonStr m a -> MonStr m b
map f (MCons ms) = MCons $ do 
                      (a, ms') <- ms
                      return ((f a), (map f ms'))

take :: (Monad m) => Int -> MonStr m a -> [m a]
take 0 _  = []
take n ms = head ms : take (n - 1) (tail ms) 

drop :: (Monad m) => Int -> MonStr m a -> MonStr m a
drop 0 ms = ms
drop n ms = drop (n - 1) (tail ms)