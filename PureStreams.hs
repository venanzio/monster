{-
   Operations on Pure Streams, defined as Identity-monsters
     Venanzio Capretta & Christopher Purdy, 2020

   Reimplementation of the Stream library by Wouter Swiestra and Bas van Dijk
-}

module PureStreams where

import MonStreams

import Control.Monad.Identity

type Stream = MonStr Identity

headS :: Stream a -> a
headS = runIdentity . headMS

tailS :: Stream a -> Stream a
tailS = runIdentity . tailMS  -- equivalent to tailMMS but simpler

outS :: Stream a -> (a, Stream a)
outS s = (headS s, tailS s) 

nats :: Applicative m => MonStr m Integer
nats = fromNat 0
  where fromNat n = n <: fromNat (n+1)

takeS :: Int -> Stream a -> [a]
takeS n s
  | n <= 0    = []
  | otherwise = headS s : takeS (n-1) (tailS s) 

dropS :: Int -> Stream a -> Stream a
dropS n s
  | n <= 0    = s
  | otherwise = dropS (n-1) (tailS s)

mapS :: (a -> b) -> Stream a -> Stream b
mapS f s = fmap f s

(!|) :: Stream a -> Integer -> a
s !| 0 = headS s
s !| n = (tailS s) !| (n - 1)