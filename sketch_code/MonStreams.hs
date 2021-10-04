-- Monadic Streams
--   Venanzio Capretta & Christopher Purdy, 2020

module MonStreams where

import Control.Applicative
import Control.Monad
--import Control.Comonad

-- Type of monadic streams
-- m is not required to be a monad

data MonStr m a = MCons (m (a , MonStr m a))

{-
  Terminology: "monster" means monadic stream, an element of: MonStr m a
  m-monster means an monadic stream under the monad,
     an element of: m (MonStr m a)
-}

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

-- Transform a monster by mapping head and tail to new head and tail
transformMS :: Functor m => 
               (a -> MonStr m a -> (b, MonStr m b)) ->
               MonStr m a -> MonStr m b
transformMS f s = MCons $ fmap (\(h,t) -> f h t) (unwrapMS s)

-- Appending an m-element in front of a stream
infixr 5 <::
(<::) :: Functor m => m a -> MonStr m a -> MonStr m a
ma <:: s = MCons (fmap (\a -> (a,s)) ma)

instance Functor m => Functor (MonStr m) where
  -- fmap :: (a -> b) -> MonStr m a -> MonStr m b
  fmap f = transformMS (\a s -> (f a, fmap f s))


-- Operations for m Applicative

-- Generic pairing operation for Applicatives
pairA :: Applicative m => m a -> m b -> m (a,b)
pairA = liftA2 (,)

-- appending an m-element in front of an m-stream
infixr 5 <:::
(<:::) :: Applicative m => m a -> m (MonStr m a) -> MonStr m a
ma <::: ms = MCons (pairA ma ms)

-- Appends a pure element to the beginning of the given monadic stream
infixr 5 <:
(<:) :: Applicative m => a -> MonStr m a -> MonStr m a
a <: s = pure a <:: s

-- Double Applicative
infix 5 <<*>>
(<<*>>) :: (Applicative m1, Applicative m2) =>
           m1 (m2 (a->b)) -> m1 (m2 a) -> m1 (m2 b)
(<<*>>) = liftA2 (<*>)

repeatMS :: Applicative m => a -> MonStr m a
repeatMS a = a <: repeatMS a

transfAppMS :: Applicative m =>
               (a -> MonStr m a -> b -> MonStr m b -> (c, MonStr m c)) ->
               MonStr m a -> MonStr m b -> MonStr m c
transfAppMS f as bs = MCons $ (\(a,as') (b,bs') -> f a as' b bs')
                              <$> unwrapMS as <*> unwrapMS bs

instance Applicative m => Applicative (MonStr m) where
  -- pure :: a -> MonStr m a
  pure = repeatMS -- constant stream

  -- (<*>) :: MonStr m (a->b) -> MonStr m a -> MonStr m b
  (<*>) = transfAppMS (\f fs a as -> (f a, fs <*> as))

{-    MCons $ (\(f,fs) (a,as) -> (f a, fs<*>as))
                      <$> (unwrapMS fs) <*> (unwrapMS as)
-}
    
    -- (headMS fs <*> headMS as) <::: (tailMS fs <<*>> tailMS as)
{- THIS DEFINITION is incorrect: doesn't satisfy the Applicative laws
   For example:
   > printTree $ pure id <*> branch [1<:leaf,2<:leaf]
   branch [ 1 <: leaf
          , 1 <: leaf
          , 2 <: leaf
          , 2 <: leaf
          ]
  This happens because we are "detaching" heads and tail and then recursing.
  We should instead "go down the diagonal" as for Monad below.
-}

-- Operations for m Monad

-- When m is a monad we can return a "pure tail"
--  the tail cannot be recombined with the head to get the same monster, since this would 
--  result in duplication of a monadic action, potentially leading to a different result
-- This function should be equivalent to (absorbMS . tailMS)
tailMMS :: Monad m => MonStr m a -> MonStr m a
tailMMS = MCons . join . fmap unwrapMS . tailMS

-- Tail for m Foldable
tailF :: (Monad m, Foldable m) => MonStr m a -> MonStr m a
tailF ma = let t = tailMS ma in
              if null t then error "Empty monadic stream" else (MCons . join . fmap unwrapMS) t
        
-- "lift" monadic actions from the elements to the stream
--  !Note! I'm not sure that this works, since the monadic action 
--         from sm is executed twice, which changes the behaviour 
--         of the monster in most circumstances
liftMS :: Monad m => MonStr m (m a) -> m (MonStr m a)
liftMS sm = do ma <- headMS sm 
               ms <- tailMS sm
               return (ma <::: liftMS ms)
               
-- "lift" monadic actions from the elements to the stream
liftMS' :: Monad m => MonStr m (m a) -> m (MonStr m a)
liftMS' sm = pure (\(ma,ms) -> ma <::: liftMS' ms) <*> unwrapMS sm

-- Monsters can "absorb" a monadic action
absorbMS :: Monad m => m (MonStr m a) -> MonStr m a
absorbMS ms = MCons . join $ fmap unwrapMS ms

-- Transform a monster by operating on "raw" head and tail
mapOutMS :: Monad m => (a -> MonStr m a -> MonStr m b) ->
                       MonStr m a -> MonStr m b
mapOutMS f s = absorbMS $ fmap (uncurry f) (unwrapMS s)

-- A "monster matrix" is a monster of monsters
type MonMatrix m a = MonStr m (MonStr m a)

-- "origin" of a monster matrix (first element of the first row)
originMM :: Monad m => MonMatrix m a -> m a
originMM mm = headMS mm >>= headMS

-- submatrix one step down the diagonal
diagonalMM :: Monad m => MonMatrix m a -> m (MonMatrix m a)
diagonalMM mm = tailMS mm >>= liftMS . fmap tailMS

-- Join operation for the MonStr monad - travels down the diagonal
--   This is incorrect: the same action is duplicated and executed many times
{-
joinMS :: Monad m => MonMatrix m a -> MonStr m a
joinMS mm = originMM mm <::: fmap joinMS (diagonalMM mm)
-}

-- Compute origin and submatrix (one step down diagonal) at the same time
joinMS :: Monad m => MonMatrix m a -> MonStr m a
joinMS mm = MCons $ 
  do (r, mm0) <- unwrapMS mm      -- first row and remaining rows
     (o,_) <- unwrapMS r          -- origin: head of first row
     let mm1 = fmap tailMMS mm0   -- delete first column
     return (o, joinMS mm1)


-- These versions seem to be the way to join the inner and outer streams resulting in the least duplication of monadic action
--  they only satisfy the monad laws for monads which have certain properties (discussed in monster_monad.tex)
joinMS' :: Monad m => MonMatrix m a -> MonStr m a
joinMS' = MCons . join . fmap (\(as, ss) -> fmap (\a -> (a, joinMS'' . fmap (absorbMS . tailMS) $ ss)) . headMS $ as) . unwrapMS 

outMS :: Functor m => MonStr m a -> (m a, m (MonStr m a))
outMS ms = (headMS ms, tailMS ms)

instance Monad m => Monad (MonStr m) where
  -- (>>=) :: MonStr m a -> (a -> MonStr m b) -> MonStr m b
  as >>= f = (joinMS' . fmap f) as

  

{- 
  It seems that a monadic stream with an underlying comonad forms a comonad itself 
   The laws are satisfied as far as I've been able to check with ad-hoc reasoning
-}
{-
instance Comonad w => Comonad (MonStr w) where
  --extract :: MonStr w a -> a
  extract = extract . headMS
  
  --duplicate :: MonStr w a -> MonStr w (MonStr w a)
  duplicate ms = MCons $ fmap (\(h,t) -> (ms, duplicate t)) (unwrapMS ms)
-}


