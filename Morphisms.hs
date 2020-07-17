{-# LANGUAGE RankNTypes #-}

module Morphisms where

import MonStreams

type MMorphism m0 m1 = forall a. m0 a -> m1 a

morphMS :: (Functor m0, Functor m1) => MMorphism m0 m1 -> MonStr m0 a -> MonStr m1 a
morphMS phi (MCons m) = MCons (phi $ fmap (\(a,s) -> (a,morphMS phi s)) m)
                                     

type PureFun a b = forall m. MonStr m a -> m b

