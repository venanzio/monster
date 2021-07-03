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
  | A module for dataflow programming
  combinators, using monsters
  
  Heavily inspired by the Lucid language
-}

import MonadicStreams
import Examples.PureStreams
import Prelude hiding (head, repeat)

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

infixr 9 ==|
(==|) :: (Applicative m, Eq a) => MonStr m a -> MonStr m a -> MonStr m Bool
ma ==| mb = (==) <$> ma <*> mb

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

-- | Whenever - I used the same policy as ifte with the monadic 
-- actions, where they are absorbed until an element is needed
--
-- This is not guarded by constructors, but would be if we used 
-- partial streams, their type being MonStr m (Maybe a)
infixr 4 `wvr`
wvr :: Monad m => MonStr m a -> MonStr m Bool -> MonStr m a
ma `wvr` mb = MCons $ do (b, mb') <- uncons mb
                         if b then do (a, ma') <- uncons ma
                                      return (a, ma' `wvr` mb')
                              else uncons $ (tailM ma) `wvr` mb'

ini :: Functor m => MonStr m a -> MonStr m a
ini = repeat . head

infixr 5 `asa`
asa :: Monad m => MonStr m a -> MonStr m Bool -> MonStr m a
ma `asa` mb = ini (ma `wvr` mb)


-- | Examples

test :: Stream Int
test = pure 1 `fby` test +| (pure 1)

mult2 :: Stream Int
mult2 = test *| (pure 2)
 
flipS :: Stream Bool
flipS = pure True `fby` notS flipS

test2 :: Stream Int
test2 = test `on` flipS

test3 :: Stream Int
test3 = test `on` pure True

s :: Stream Int
s = 1 <: 2 <: 3 <: 4 <: (pure 5)

pwr2 :: Stream Int
pwr2 = pure 1 `fby` (pwr2 *| pure 2)

fib :: Stream Int
fib = pure 0 `fby` ((pure 1 `fby` fib) +| fib)


-- | Partial monadic streams, and related combinators
-- Working out semantics for this is probably needed
-- to figure out the partial versions of most combinators
--
-- I haven't yet found a paper on partial streams (otherwise 
-- called hiatonic streams) extending the wvr, on, asa, etc.

newtype PMonStr m a = PMS { mstr :: MonStr m (Maybe a)}

instance Functor m => Functor (PMonStr m) where
  fmap f (PMS ms) = PMS $ fmap (fmap f) ms
  
instance Applicative m => Applicative (PMonStr m) where
  pure a = PMS $ pure (pure a)
  (PMS mf) <*> (PMS ma) = PMS $ mf <<*>> ma
  

liftPMS :: (MonStr m (Maybe a) -> MonStr m (Maybe b)) -> PMonStr m a -> PMonStr m b
liftPMS f (PMS pms) = PMS (f pms)

nosig :: Applicative m => PMonStr m a
nosig = PMS (pure Nothing)

notS' :: Functor m => PMonStr m Bool -> PMonStr m Bool
notS' = fmap not

infixr 5 +||
(+||) :: (Applicative m, Num a) => PMonStr m a -> PMonStr m a -> PMonStr m a
ma +|| mb = (+) <$> ma <*> mb

infixr 5 -|| 
(-||) :: (Applicative m, Num a) => PMonStr m a -> PMonStr m a -> PMonStr m a
ma -|| mb = (-) <$> ma <*> mb

infixr 9 *||
(*||) :: (Applicative m, Num a) => PMonStr m a -> PMonStr m a -> PMonStr m a
ma *|| mb = (*) <$> ma <*> mb

infixr 9 //||
(//||) :: (Applicative m, Integral a) => PMonStr m a -> PMonStr m a -> PMonStr m a
ma //|| mb = div <$> ma <*> mb

infixr 9 /|| 
(/||) :: (Applicative m, Fractional a) => PMonStr m a -> PMonStr m a -> PMonStr m a
ma /|| mb = (/) <$> ma <*> mb

infixr 9 ==||
(==||) :: (Applicative m, Eq a) => PMonStr m a -> PMonStr m a -> PMonStr m Bool
ma ==|| mb = (==) <$> ma <*> mb

infixr 4 `fby'`
fby' :: Functor m => PMonStr m a -> PMonStr m a -> PMonStr m a
(PMS ma) `fby'` (PMS mb) = PMS . MCons $ fmap (\p -> (fst p, mb)) (uncons ma)

{-

infixr 9 `on'`
on' :: Monad m => PMonStr m a -> PMonStr m Bool -> PMonStr m a
(PMS ma) `on'` (PMS mb) = PMS (onaux ma mb)
                          where onaux ma mb = MCons $ do (a, ma') <- (uncons ma)
                                                         (b, mb') <- (uncons mb)
                                                         return (case b of
                                                                    Nothing -> (Nothing, onaux ma mb')
                                                                    Just b  -> (a, if b then (onaux ma' mb') else (onaux ma mb')))

-- | If then else - should the monadic action of the other stream 
-- be run if its value is not used, or should it be pushed back a
-- level with tailM?
ifte' :: Monad m => MonStr m Bool -> MonStr m a -> MonStr m a -> MonStr m a
ifte' mb mt mf = MCons $ do (b, mb') <- uncons mb
                            if b 
                               then do (t, mt') <- uncons mt
                                       return (t, ifte mb' mt' (tailM mf))
                               else do (f, mf') <- uncons mf
                                       return (f, ifte mb' (tailM mt) mf')

-- | Whenever - I used the same policy as ifte with the monadic 
-- actions, where they are absorbed until an element is needed
--
-- This is not guarded by constructors, but would be if we used 
-- partial streams, their type being MonStr m (Maybe a)
infixr 4 `wvr'`
wvr' :: Monad m => MonStr m a -> MonStr m Bool -> MonStr m a
ma `wvr'` mb = MCons $ do (b, mb') <- uncons mb
                          if b then do (a, ma') <- uncons ma
                                       return (a, ma' `wvr'` mb')
                               else uncons $ (tailM ma) `wvr'` mb'

ini' :: Functor m => MonStr m a -> MonStr m a
ini' = repeat . head

infixr 5 `asa'`
asa' :: Monad m => MonStr m a -> MonStr m Bool -> MonStr m a
ma `asa'` mb = ini (ma `wvr'` mb)

-}