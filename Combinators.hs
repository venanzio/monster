{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}

module Combinators where

import Control.Monad
import Control.Comonad
import Control.Comonad.Store
import Control.Monad.Trans
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
mfunc2 :: Functor m => (a -> b -> c) -> MonStr m (a, b) -> MonStr m c
mfunc2 = fmap . uncurry 

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

insertAct :: Monad m => Int -> m a -> MonStr m a -> MonStr m a
insertAct 0 ma mas = MCons . join $ (\(h,t) -> fmap (const (h,t)) ma) <$> unwrapMS mas
insertAct n ma mas = MCons $ (\(h,t) -> (h, insertAct (n-1) ma t)) <$> unwrapMS mas


liftNat :: (Functor m, Functor n) => (forall a. (m a -> n a)) -> MonStr m a -> MonStr n a
liftNat f (MCons ma) = MCons $ f (fmap (\(a, s) -> (a, liftNat f s)) ma)

liftMT :: (MonadTrans t, Monad m, Functor (t m)) => MonStr m a -> MonStr (t m) a
liftMT = liftNat lift

{-

-- Monadic stream functions using monadic streams

-- functorial in a, contra-functorial in r
-- This is just the ReaderT monad
newtype MArr m r a = MArr (r -> m a)

instance (Functor m) => Functor (MArr m a) where
  fmap fab (MArr ra) = MArr (\r -> fmap fab (ra r))

instance (Applicative m) => Applicative (MArr m a) where
  pure a = MArr (\_ -> pure a)
  (MArr rf) <*> (MArr ra) = MArr (\r -> (rf r) <*> (ra r))
  
instance (Monad m) => Monad (MArr m a) where 
  (MArr ra) >>= arb = MArr (\r -> join (fmap (\(MArr f) -> f r) (fmap arb (ra r))) )

type MSF m a b = MonStr (MArr m a) b

preComp :: (Functor m) => forall a b c. (a -> b) -> (MArr m b c -> MArr m a c)
preComp f (MArr bc) = MArr (\a -> bc (f a))

liftNatPreComp f = liftNat (preComp f)

-}

-- Monad specific operations on monadic streams

stepM :: Monad m => (a -> m a) -> MonStr m a -> MonStr m a
stepM f (MCons ms) = MCons $ join $ fmap (\(a, ms') -> fmap (\a' -> (a', ms')) (f a)) ms



data MSF m a b = MSF (MonStr m a -> MonStr m b)

data MSF' m a b = MSF' ((Store (m a) Int) -> m b)



-- Isomorphism between monadic streams and functions (Int -> m a) - DOESN'T COMMUTE

phi :: Monad m => MonStr m a -> Int -> m a
phi ms 0 = headMS ms
phi ms n = phi ((MCons . join . fmap unwrapMS . tailMS) ms) (n - 1)

-- Just a special case of unfold (co-iterator)
psi :: Functor m => (Int -> m a) -> MonStr m a
psi f = psi' f 0
        where psi' f n = MCons $ fmap (\a -> (a, psi' f (n + 1))) (f n)




phi' :: Comonad w => MonStr w a -> Int -> w a
phi' ms 0 = headMS ms
phi' ms n = phi' ((MCons . join . fmap unwrapMS . tailMS) ms) (n - 1)

-- Just a special case of unfold (co-iterator)
psi' :: Comonad w => (Int -> w a) -> MonStr w a
psi' f = psi'' f 0
         where psi'' f n = MCons $ fmap (\a -> (a, psi'' f (n + 1))) (f n)




