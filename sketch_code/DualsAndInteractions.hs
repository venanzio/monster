{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}

module DualsAndInteractions where

import MonStreams
import ComonadicStream

import Data.Either
import Control.Monad
import Control.Comonad
import Control.Applicative

-- This is like the dual to comonadic streams - a monadic list?
data MonCoStr m a = MonCo (m (Either a (MonCoStr m a)))

instance Functor f => Functor (MonCoStr f) where
  fmap f (MonCo mco) = MonCo $ fmap (caseE f (fmap f)) mco
                       where caseE f f' e = case e of 
                                               Left  a -> Left  (f a)
                                               Right b -> Right (f' b)

instance Applicative m => Applicative (MonCoStr m) where
  pure = return
  (<*>) = ap

{-
  Very similar to the free monad instance

  The comonadic stream comonad instance only requires functor
   laws for duplicate, but requires the functor to have coalgebras
   for extract
  
  The same can be said for the bind instance here, it only requires
   functor laws - however return requires the functor to have
   algebras (which an the Applicative instance supplies with pure)

-}
instance Applicative m => Monad (MonCoStr m) where
  return = MonCo . pure . Left  
  -- (>>=) :: MonCoStr m a -> (a -> MonCoStr m b) -> MonCoStr m b
  -- This works by sequencing the actions of the first with the second
  (MonCo mca) >>= f = MonCo $ fmap aux mca
                      where aux (Left a)     = Right (f a)
                            aux (Right mca') = Right (mca' >>= f)

-- Collapses the stack of actions into a single action
-- De-intensionalises the monad
lastMC :: Monad m => MonCoStr m a -> m a
lastMC (MonCo mco) = join $ fmap aux mco
                     where aux (Left a)     = return a
                           aux (Right mco') = lastMC mco'
                           
                           
-- Tarmo Uustalu monad + comonad interaction laws
--  Needs to satisfy particular commutation laws
type MCLaw m w = forall a b. (m a, w b) -> (a, b)

{-
psi :: MonCoLaw m w

mma :: m (m a)
wwb :: w (w b)
ma :: m a
wb :: w b
a :: a
b :: b

Laws:

(a, extract wb) == psi (return a, wb)
psi (psi (mma, duplicate wb)) == psi (join mma, wb)

-}

-- Walks down the stream and list in parallel
interaction :: (Functor m, Functor w) => MCLaw m w -> MCLaw (MonCoStr m) (MonStr w)
interaction f (MonCo ms, MCons ws) = case f (ms, ws) of
                                       (Left  a  , (b, ws')) -> (a, b)
                                       (Right ms', (b, ws')) -> interaction f (ms', ws')





