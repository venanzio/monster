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
tailS = runIdentity . tailMS

nats :: Stream Integer
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











{- The following is from "The Construction of Infinity"
   It needs to be adapted to the implementation of pure streams
      as Identity-monsters
   It should be put in a separate file
-}

-- A language for pure streams and element terms in many stream arguments

data STerm = STArg Int | STTail STerm | STCons ETerm STerm | STRec Int [STerm]
data ETerm = ETHead STerm

{- A system of pure stream equations is given by a list of terms
   (STRec k) refers to the k-th function in the system
-}

-- Function Corresponding to a pure stream equation systems
-- Arguments are given as a list of streams (infinite lists)
funST :: [STerm] -> [[[a]] -> [a]]
funST terms = solveST terms (funST terms)

solveST :: [STerm] -> [[[a]] -> [a]] -> [[[a]] -> [a]]
solveST terms funs = map funSTerm terms
  where funSTerm (STArg i) alpha = alpha!!i
        funSTerm (STTail s) alpha = tail (funSTerm s alpha)
        funSTerm (STCons e s) alpha = (funETerm e alpha) : (funSTerm s alpha)
        funSTerm (STRec k ts) alpha = (funs!!k) (map (\t -> funSTerm t alpha) ts)

        funETerm (ETHead s) alpha = head (funSTerm s alpha)

-- When we know exactly how many arguments a function has

funST1 :: [STerm] -> Int -> [a] -> [a]
funST1 terms k alpha = ((funST terms)!!k) [alpha]

funST2 ::  [STerm] -> Int -> [a] -> [a] -> [a]
funST2 terms k alpha1 alpha2 = ((funST terms)!!k) [alpha1,alpha2]

-- Examples

-- Evens and Odds
--   evens s = head s : odd (tail s)
--   odds s = evens (tail s)
eoEq :: [STerm]
eoEq = [STCons (ETHead (STArg 0)) (STRec 1 [STTail (STArg 0)]),
        STRec 0 [STTail (STArg 0)]
       ]

evens = funST1 eoEq 0
odds = funST1 eoEq 1

-- Interleave
--   intrlv s1 s2 = head s1 : intrlv s2 (tail s1)
intrlvEq :: [STerm]
intrlvEq = [STCons (ETHead (STArg 0)) (STRec 0 [STArg 1,STTail (STArg 0)])]

intrlv = funST2 intrlvEq 0
