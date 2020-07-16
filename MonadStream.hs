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
import Stream hiding ((<:>), head, tail, map, take, drop, inits, tails)

data MonStr m a = MCons (m (a , MonStr m a))

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

map :: (Monad m) => (a -> b) -> MonStr m a -> MonStr m b
map f (MCons ms) = MCons $ do 
                      (a, ms') <- ms
                      return ((f a), (map f ms'))

-- MonStr typeclass instances
-----------------------------
instance (Monad m) => Functor (MonStr m) where
  -- fmap :: (a -> b) -> MonStr m a -> MonStr m b
  fmap = map
  
instance (Monad m) => Applicative (MonStr m) where
  -- pure :: a -> MonStr m a
  pure a = MCons $ pure (a, pure a)
  
  -- (<*>) :: MonStr m (a->b) -> MonStr m a -> MonStr m b
  MCons fs <*> MCons as = MCons $ do 
                              (f, fs') <- fs
                              (a, as') <- as
                              return (f a, fs' <*> as')
    
instance (Monad m) => Monad (MonStr m) where
  -- (>>=) :: MonStr m a -> (a -> MonStr m b) -> MonStr m b
  ms >>= f = joinMS $ fmap f ms
             where
                joinMS :: (Monad m) => MonStr m (MonStr m a) -> MonStr m a
                joinMS ~ms = join (fmap head (head ms)) <&> joinMS (map tail (tail ms))

-----------------------------

-- Further helper functions for monadic streams
-----------------------------------------------

-- Returns a Stream of all possible initial portions of the given MonStr
inits :: (Monad m) => MonStr m a -> Stream ([m a])
inits ms = Cons [] (fmap (head ms :) (inits (tail ms)))

-- Returns a Stream of all possible tails of
tails :: (Monad m) => MonStr m a -> Stream (MonStr m a)
tails ms = Cons ms (tails (tail ms))

take :: (Monad m) => Int -> MonStr m a -> [m a]
take 0 _  = []
take n ms = head ms : take (n - 1) (tail ms) 

drop :: (Monad m) => Int -> MonStr m a -> MonStr m a
drop 0 ms = ms
drop n ms = drop (n - 1) (tail ms)


-- Concrete implementations for testing functions in ghci
---------------------------------------------------------
type LList a = MonStr Maybe a

nil :: LList a
nil = MCons Nothing

cons :: a -> LList a -> LList a
cons a l = MCons (Just (a,l))

llist :: [a] -> LList a
llist = foldr cons nil

fromL :: LList a -> [a]
fromL (MCons Nothing) = []
fromL (MCons (Just (a,l))) = a : fromL l

l1 :: LList Int
l1 = llist [0..9]

timesTablesNot9s :: Int -> LList Int
timesTablesNot9s n = MCons $ case (n `mod` 9) of 0 -> Just (n - 1, timesTablesNot9s (n - 1))
                                                 _ -> Just (n, timesTablesNot9s (n*n))
                                       