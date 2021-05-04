{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}

module ComonadicStreams where

import Prelude hiding (head, tail)

import MonadicStreams

import Examples.PureStreams
import Control.Comonad
import Control.Comonad.Store


headC :: Comonad w => MonStr w a -> a
headC (MCons s) = fst (extract s)

tailC :: Comonad w => MonStr w a -> MonStr w a
tailC (MCons s) = snd (extract s)

takeC :: Comonad w => Int -> MonStr w a -> [a]
takeC 0 s = []
takeC n s = headC s : takeC (n-1) (tailC s)

-- | Generates a comonadic stream using a coKleisli arrow and a starting environment
-- as a seed
iterateC :: Comonad w => (w a -> a) -> w a -> MonStr w a
iterateC f wa = let ~wa' = f <<= wa in MCons $ fmap (\a -> (a, iterateC f wa')) wa

-- | Evaluates all of the environments in the stream sequentially with the given coKleisli
-- arrow
collapseAllC :: Comonad w => (w a -> b) -> MonStr w a -> Stream b
collapseAllC f ws = MCons $ return (f (head ws), collapseAllC f (extract (tail ws)))

-- | Appending to a comonster
(<@:) :: Comonad w => a -> MonStr w a -> MonStr w a
a <@: (MCons ws) = MCons $ fmap (\wa -> (a, MCons wa)) $ duplicate ws

-- | Extracting at a given index of a comonster
(!@!) :: Comonad w => MonStr w a -> Int -> w a
ws !@! 0 = head ws
ws !@! n = tailC ws !@! (n - 1)

