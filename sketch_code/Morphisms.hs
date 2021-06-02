{-# LANGUAGE RankNTypes #-}

module Morphisms where

import MonStreams
import Control.Applicative

-- Monad Morphisms

type MMorphism m0 m1 = forall a. m0 a -> m1 a

-- Examples of morphisms

maybeList :: MMorphism Maybe []
maybeList Nothing = []
maybeList (Just a) = [a]

listMaybeL :: MMorphism [] Maybe
listMaybeL [] = Nothing
listMaybeL (x:xs) = Just x

listMaybeR :: MMorphism [] Maybe
listMaybeR [] = Nothing
listMaybeR xs = Just (last xs)

listToZipL :: MMorphism [] ZipList
listToZipL x = ZipList x

-- Lifting of monad morphisms to monadic streams

morphMS :: (Functor m0, Functor m1) =>
           MMorphism m0 m1 -> MonStr m0 a -> MonStr m1 a
morphMS phi (MCons m) = MCons (phi $ fmap (\(a,s) -> (a,morphMS phi s)) m)
                                     
-- Pure stream functions: polymorphic on the monad

type PureFun a b = forall m. Monad m => MonStr m a -> m b

-- A pure function that returns the index of the first non-decreasing element

nodec :: Integral a => PureFun a a
nodec (MCons m) = do (x,s) <- m
                     nodec' x s
  where nodec' x (MCons m') = do
          (x',s') <- m'
          if x <= x' then return 1
                     else fmap (+1) (nodec' x' s')
