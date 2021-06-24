{-# LANGUAGE FlexibleInstances #-}

module Combinators (
  (+|),
  (-|),
  (*|),
  (//|),
  (/|),
  fby
  ) where

{-
 
  A module for dataflow programming
  combinators, using monsters
  
  Heavily inspired by the Lucid language
 
-}

import MonadicStreams
import Examples.PureStreams

{-
Partial monadic streams

newtype PMonStr m a = PMS (MonStr m (Maybe a))

instance Functor m => Functor (PMonStr m) where
  fmap f (PMS ms) = PMS $ fmap (fmap f) ms
-}

notS :: Functor m => MonStr m Bool -> MonStr m Bool
notS ma = fmap not ma

infixr 5 +| 
(+|) :: (Applicative m, Num a) => MonStr m a -> MonStr m a -> MonStr m a
ma +| mb = (+) <$> ma <*> mb

infixr 5 -| 
(-|) :: (Applicative m, Num a) => MonStr m a -> MonStr m a -> MonStr m a
ma -| mb = (-) <$> ma <*> mb

infixr 9 *| 
(*|) :: (Applicative m, Num a) => MonStr m a -> MonStr m a -> MonStr m a
ma *| mb = (*) <$> ma <*> mb

infixr 9 //| 
(//|) :: (Applicative m, Integral a) => MonStr m a -> MonStr m a -> MonStr m a
ma //| mb = div <$> ma <*> mb

infixr 9 /| 
(/|) :: (Applicative m, Fractional a) => MonStr m a -> MonStr m a -> MonStr m a
ma /| mb = (/) <$> ma <*> mb

infixr 4 `fby`
fby :: Functor m => MonStr m a -> MonStr m a -> MonStr m a
(MCons ma) `fby` mb = MCons $ fmap (\p -> (fst p, mb)) ma

infixr 9 `on`
on :: Monad m => MonStr m a -> MonStr m Bool -> MonStr m a
(MCons ma) `on` (MCons mb) = MCons $ do (a, ma') <- ma
                                        (b, mb') <- mb
                                        return (a, if b then (ma' `on` mb') else ((MCons ma) `on` mb'))

-- | If then else - should the monadic action of the other stream 
-- be run if its value is not used, or should it be pushed back a
-- level with tailM?
ifte :: Monad m => MonStr m Bool -> MonStr m a -> MonStr m a -> MonStr m a
ifte mb mt mf = MCons $ do (b, mb') <- uncons mb
                           if b 
                              then do (t, mt') <- uncons mt
                                      return (t, ifte mb' mt' (tailM mf))
                              else do (f, mf') <- uncons mf
                                      return (f, ifte mb' (tailM mt) mf')

-- | Whenever
infixr 4 `wvr`
wvr :: Monad m => MonStr m a -> MonStr m Bool -> MonStr m a
ma `wvr` mb = MCons $ do (b, mb') <- uncons mb
                         if b then do (a, ma') <- uncons ma
                                      return (a, ma' `wvr` mb')
                              else uncons $ (tailM ma) `wvr` mb'


-- | Examples

test :: Stream Int
test = pure 1 `fby` test +| (pure 1)

mult2 :: Stream Int
mult2 = test *| (pure 2)
-- testSq = pure 2 `fby` testSq *| (pure 2)
 
flipS :: Stream Bool
flipS = pure True `fby` notS flipS

test2 :: Stream Int
test2 = test `on` flipS

test3 :: Stream Int
test3 = test `on` pure True


s :: Stream Int
s = 1 <: 2 <: 3 <: 4 <: (pure 5)

k :: Stream Int
k = pure 1 `fby` (k *| pure 2)