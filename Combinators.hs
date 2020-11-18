module Combinators where
 
import Control.Monad
import Control.Applicative
import MonStreams
import Operations

-- Takes a monadic action and turns it into a monster 
streamify :: Monad m => m a -> MonStr m a
streamify ma = MCons $ do a <- ma
                          return (a, streamify ma)

-- Takes two monadic streams, and combines the monadic actions at each point in the stream, returning the elements from the second stream
--  this can be used to interleave the actions in two IO monsters for example (the actions in each stream are both run at each step)
infixr 5 >>>
(>>>) :: Monad m => MonStr m a -> MonStr m b -> MonStr m b
mas >>> mbs = MCons $ do (a, as) <- unwrapMS mas
                         fmap (\(e,s) -> (e, as >>> s)) (unwrapMS mbs)

-- Pairs elements of two streams
(&&&) :: Applicative m => MonStr m a -> MonStr m b -> MonStr m (a, b)
ma &&& mb = liftA2 (,) ma mb

-- Lifts a binary functions to act on monsters of pairs
mfunc2 :: Monad m => (a -> b -> c) -> MonStr m (a, b) -> MonStr m c
mfunc2 f = fmap (uncurry f)
    
-- Takes a monadic action and joins it with each action in the given monster, executing the new monadic action second
--  throws away the return value of the given monadic action
infixl 5 |:>
(|:>) :: Monad m => MonStr m a -> m b -> MonStr m a
mas |:> ma = MCons . join $ (\(a,s) -> fmap (\_ -> (a, s |:> ma)) ma) <$> unwrapMS mas

-- Takes a monadic action and joins it with each action in the given monster, executing the new monadic action first
--  throws away the return value of the given monadic action
infixr 5 |:<
(|:<) :: Monad m => m a -> MonStr m b -> MonStr m b
ma |:< mas = MCons . join $ (\_ -> fmap (\(a, s) -> (a, ma |:< s)) (unwrapMS mas)) <$> ma


-- Takes a monadic action and joins it with each action in the given monster, executing the new monadic action second
--  modifies the result value with the function inside the new monadic action
infixl 5 $:>
($:>) :: Monad m => MonStr m a -> m (a -> b) -> MonStr m b
mas $:> mf = MCons . join $ (\(a,s) -> fmap (\f -> (f a, s $:> mf)) mf) <$> unwrapMS mas

-- Takes a monadic action and joins it with each action in the given monster, executing the new monadic action first
--  modifies the result value with the function inside the new monadic action
infixr 5 $:<
($:<) :: Monad m => m (a -> b) -> MonStr m a -> MonStr m b
mf $:< mas = MCons . join $ (\f -> fmap (\(a, s) -> (f a, mf $:< s)) (unwrapMS mas)) <$> mf
                  
