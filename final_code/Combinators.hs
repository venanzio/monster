module Combinators (
  (+|),
  (-|),
  (*|),
  (//|),
  (/|),
  (>:>)
  ) where

{-
 
  A module for dataflow programming
  combinators, using monsters
  
  Heavily inspired by Lucid
 
-}

import MonadicStreams
import Examples.PureStreams

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

infixr 9 >:>
(>:>) :: Applicative m => MonStr m a -> MonStr m a -> MonStr m a
(MCons ma) >:> mb = MCons $ fmap (\p -> (fst p, mb)) ma
