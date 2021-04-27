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
import PureStreams

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


-- Lifting kleisli and cokleisli morphisms to monsters

{-

liftK takes each value in a stream, and decorates it

  - []
    Takes a non-deterministic computation, and a stream of values to
    evaluate, and turns it in to a tree where paths are potential streams of values
    resulting from the non-deterministic computation

  - Maybe
    Takes a potentially failing computation, and a stream of values,
    and returns the stream of results, but cut off at the first failed
    computation

  - State
    Takes a computation specification, and a stream of values,
    and produces a computation specification for each value, and 
    threads them together. This essentially results in a Mealy machine

  - IO
    Takes a impure computation specification, and a stream of inputs, and returns
    an impure process (essentially a Mealy machine with IO side-effects) 

  - Reader (e -> a)
    Takes a indexed family of ways of decomposing an environment, and a stream of
    indicies, and produces the stream of functions that take an environment, and 
    give an evaluation of the environment along with the next evaluation function.
    This is like a machine whose output depends only on an input, with no internal
    state, essentially just a stream of continuations

  - Cont ((a -> r) -> r) 
    A function from (a -> (b -> r) -> r) is the same as a function (b -> r) -> (a -> r),
    which can be thought of as a function (a -> r) with a 'hole' that needs a (b -> r) to
    fill it.
    
    Kleisli compisition is then ((b -> r) -> (a -> r)) -> ((c -> r) -> (b -> r)) -> (c -> r) -> (a -> r)
    essentially the (b -> r) needed to fill the first hole is assumed as given, if and when a function
    from (c -> r) is given.

    MonStr Cont a = ((a, MonStr Cont a) -> r) -> r = ((a, ((a, MonStr Cont a) -> r) -> r) -> r) -> r

    Work out semantics for Cont-monster
    Taking a finite substream amounts to chopping the nested continuations off at a certain point, thus
    giving a sub-computation of the whole infinite computation.

    There should be a way to pass a function (a -> r) to the monster such that all continuations are
    evaluated, since any nested continuations can be collapsed to (a -> r) -> r. There is, see below.

    Each suspended computation in the stack is generated by a different seed, and is waiting on a 
    continuation to be supplied to fully evaluate it. Once the continuation is supplied...?
    
-}
liftK :: Functor m => (a -> m b) -> Stream a -> MonStr m b
liftK f s = let (h, t) = outS s in 
               MCons $ fmap (\a -> (a, liftK f t)) (f h)
               
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
   fmap f (Cont inC) = Cont $ \out -> inC (out . f)
   
instance Applicative (Cont r) where
   pure = return
   (Cont f) <*> ca = Cont $ \out -> f (\ab -> (runCont (fmap ab ca)) out)
   
instance Monad (Cont r) where
   return x = Cont $ \out -> out x
   c >>= f = joinCont (fmap f c)
   

{-
  
This is just saying:
 If you can complete the sub-computation (m) with some continuation (out),
 and completing this completes the bigger computation (c), then out completes c
  
        ________
       |__out___|
           |
     __    |     __________________
  __|  |___V____|            m     |________________
 |  |______________________________|           c    |
 |__________________________________________________|
 


-}
joinCont :: Cont r (Cont r a) -> Cont r a
joinCont (Cont c) = Cont $ \out -> c (\m -> (runCont m) out)


contComp :: (a -> Cont r b) -> (b -> Cont r c) -> a -> Cont r c
contComp ab bc a = Cont $ \out -> runCont (ab a) $ (flip (runCont . bc) $ out)


toWordInt :: Integer -> String
toWordInt 0 = "zero"
toWordInt 1 = "one"
toWordInt 2 = "two"
toWordInt 3 = "three"
toWordInt n = if n < 0 then "not particularly positive" else "bigger than three"

seed :: Integer -> Cont String String
seed n = Cont $ \out -> (show n) ++ "|" ++ out (toWordInt n) ++ "|" ++ (show n) ++ "|" ++ out (toWordInt n) ++ "|" ++ (show n)

contStr = liftK seed nats

{- 

You can't define a runner for this monadic stream, but you could define a "partial runner"

  runCont (takeMMS 2 contStr) concat - this runs the stream cut off at the 2nd element

-}


{-

A comonad is like a machine, who's internal state can be changed with some machine specific
buttons. In the case of comonsters, you can change the state by extracting the next context 
from the current one (w a -> w a), and compute a value from the current context (machine
state) by using extract, which extracts the value from the first context in the stream.
              
In this sense, a comonster is a stream of future states of a comonad, where each next state
is deduced from the current.

lifecK takes each future context and evaluates it, producing a stream of results from the evaluated 
contexts.

  - Store ((s -> a) , s)
    This takes a store dependant function, and evaluates the store at each context in
    the monster, producing a stream of results 

    An infinitely (discretely) changing context is evaluated by some arrow at every
    time step

  - Stream (a :< (a :< (a :< ...)
    Takes a function dependant on future values in a stream, and evaluates it at every
    state of the stream as stored in the monster.

  - Conways game of life

-}

liftcK :: Comonad w => (w a -> b) -> MonStr w a -> Stream b 
liftcK f ms = f (headMS ms) <: liftcK f (extract (tailMS ms))



-- Essentially variations of fmap where the function modifies the container

stepW :: Comonad w => (w a -> b) -> MonStr w a -> MonStr w b
stepW f (MCons ms) = MCons $ fmap (\ws -> (f (fmap fst ws), stepW f $ extract (fmap snd ws))) (duplicate ms)

stepM :: Monad m => (a -> m b) -> MonStr m a -> MonStr m b
stepM f (MCons ms) = MCons $ join (fmap (\(h, t) -> fmap (\b -> (b, stepM f t)) (f h)) ms)

-- Functor needs to have a tensorial strength
step :: Functor m => (forall c. m (a, c) -> m (b, c)) -> MonStr m a -> MonStr m b
step f (MCons ms) = MCons $ fmap (\(h, t) -> (h, step f t)) (f ms)



data MSF m a b = MSF (MonStr m a -> MonStr m b)
-- THESE ARE NOT THE SAME
data MSF' m a b = MSF' ((Store (m a) Int) -> m b)



-- Isomorphism between monadic streams and functions (Int -> m a)? - DOESN'T COMMUTE

phi :: Monad m => MonStr m a -> Int -> m a
phi ms 0 = headMS ms
phi ms n = phi ((MCons . join . fmap unwrapMS . tailMS) ms) (n - 1)

-- Just a special case of unfold (co-iterator)
psi :: Functor m => (Int -> m a) -> MonStr m a
psi f = psi' f 0
        where psi' f n = MCons $ fmap (\a -> (a, psi' f (n + 1))) (f n)








