{-
   Systems of stream equations, generalised for any monadic stream MonStr m a (with m being a monad)
     Venanzio Capretta & Christopher Purdy, 2020
-}

import MonStreams
import Operations

data STerm = STArg Int | STTail STerm | STCons ETerm STerm | STRec Int [STerm]
data ETerm = ETHead STerm

-- Function Corresponding to a monadic stream equation systems
-- Arguments are given as a list of monadic streams
funST :: (Monad m) => [STerm] -> [[MonStr m a] -> MonStr m a]
funST terms = solveST terms (funST terms)

solveST :: (Monad m) => [STerm] -> [[MonStr m a] -> MonStr m a] -> [[MonStr m a] -> MonStr m a]
solveST terms funs = map funSTerm terms
  where funSTerm (STArg i) alpha = alpha !! i
        funSTerm (STTail s) alpha = tailMMS (funSTerm s alpha)
        funSTerm (STCons e s) alpha = (funETerm e alpha) <:: (funSTerm s alpha)
        funSTerm (STRec k ts) alpha = (funs !! k) (map (\t -> funSTerm t alpha) ts)

        funETerm (ETHead s) alpha = headMS (funSTerm s alpha)

-- When we know exactly how many arguments a function has

funSTN :: (Monad m) => [STerm] -> Int -> [MonStr m a]
funSTN terms k alphas = ((funST terms) !! k) alphas

funST1 :: (Monad m) => [STerm] -> Int -> MonStr m a -> MonStr m a
funST1 terms k alpha = ((funST terms) !! k) [alpha]

funST2 :: (Monad m) => [STerm] -> Int -> MonStr m a -> MonStr m a -> MonStr m a
funST2 terms k alpha1 alpha2 = ((funST terms) !! k) [alpha1, alpha2]

-- Examples ------------

-- It is now possible to run the example functions on monadic streams (like this one below)
natsLess10 :: MonStr Maybe Integer
natsLess10 = llist [0..9]

-- Evens and Odds
--   evens s = head s : odds (tail s)
--   odds s = evens (tail s)
eoEq :: [STerm]
eoEq = [STCons (ETHead (STArg 0)) (STRec 1 [STTail (STArg 0)]),
        STRec 0 [STTail (STArg 0)]
       ]

evens :: (Monad m) => MonStr m a -> MonStr m a
evens = funST1 eoEq 0

odds :: (Monad m) => MonStr m a -> MonStr m a
odds = funST1 eoEq 1

-- Interleave
--   intrlv s1 s2 = head s1 : intrlv s2 (tail s1)
intrlvEq :: [STerm]
intrlvEq = [STCons (ETHead (STArg 0)) (STRec 0 [STArg 1,STTail (STArg 0)])]

intrlv :: (Monad m) => MonStr m a -> MonStr m a -> MonStr m a
intrlv = funST2 intrlvEq 0