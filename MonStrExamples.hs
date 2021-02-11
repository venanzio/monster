{-
  Monadic Streams
  Instantiations with various monads
-}

{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}

module MonStrExamples where

import MonStreams
import ComonadicStream
import PureStreams
import Operations
import Morphisms
import Combinators

import Control.Applicative
import Control.Monad
import Control.Comonad

import Control.Monad.State
import Data.Foldable
import System.IO.Unsafe
import Data.List

-- Example: Lazy lists - Maybe Monad
------------------------------------

type LList a = MonStr Maybe a

nil :: LList a
nil = MCons Nothing

cons :: a -> LList a -> LList a
cons a l = MCons (Just (a,l))

llist :: [a] -> LList a
llist = foldr cons nil

-- This is no longer necessary: use toList from Foldable
fromL :: LList a -> [a]
fromL = toList

-- Example to test functions on
--   Venanzio comment: this can be done by (llist [1..9])
natsLess10 :: MonStr Maybe Integer
natsLess10 = llist [0..9]

nats2To18 :: MonStr Maybe Integer
nats2To18 = llist [2..18]

natsS :: MonStr Maybe Integer
natsS = llist [0..]

sentence :: MonStr Maybe Char
sentence = llist (unwords ["we","want","a","shrubbery",""])

-- Examples to test the correctness of the Monad instantiation:
--   verify that joinMS takes the diagonal

-- taking a submatrix of size n x m
subMatrix :: [[a]] -> Int -> Int -> [[a]]
subMatrix xss n m = Prelude.take n (fmap (Prelude.take m) xss)

toMM :: [[a]] -> MonMatrix Maybe a
toMM = llist . fmap llist

toMatrix :: MonMatrix Maybe a -> [[a]]
toMatrix = fromL . fmap fromL

-- list of lists of coordinates
xys :: [[(Int,Int)]]
xys = [[(x,y) | y <- [0..] ] | x <- [0..] ]

xyLL :: MonMatrix Maybe (Int,Int)
xyLL = llist (fmap llist xys)

-- fromL $ joinMS xyLL
--   correctly gives the diagonal: (0,0),(1,1),(2,2),(3,3),..]

-- Tests to verify the 3rd monad law

natsa = return 1 >>= (\n -> llist [n..])

natsb = (\n -> llist [n..]) 1

squares = (natsS >>= (\n -> llist [n..])) >>= (\n -> fmap (*n) natsS)

squares' = natsS >>= (\x -> ((\n -> llist [n..]) x) >>= (\n -> fmap (*n) natsS))

showMonStr :: Applicative m => MonStr m a -> m String
showMonStr ma = fmap (\_ -> "a MonStr") (unwrapMS ma)

-- Arbitrarily branching trees - List Monad
-------------------------------------------

type Tree a = MonStr [] a
    
showTree :: Show a => Tree a -> String
showTree = intercalate "\n" . stList
{-
           intercalate "\n" . stList
  where -- stList :: Tree a -> [String]
        stList t = stPairList (unwrapMS t)
        -- stPair :: Char -> (a,Tree a) -> [String]
        stPair c (a,t) = (c:show a ++ " <:") :
                       map (prefix a ++) (stList t)
        prefix a = take (length (show a) + 2) (repeat ' ') ++ "| "
        -- stPairList :: [(a,Tree a)] -> [String]
        stPairList [] = ["leaf"]
        stPairList (l:ls) = stPair '[' l : map (stPair ',') ls ++ ["]"] 
-}

strListPrefixes :: String -> String -> [String] -> [String]
strListPrefixes s0 snext [] = []
strListPrefixes s0 snext (s:ls) = (s0++s) : map (snext++) ls

stPair :: Show a => String -> (a,Tree a) -> [String]
stPair pre (a,t) = let prefix = pre ++ show a ++ " <: "
                       blanks = replicate (length prefix) ' ' 
                   in strListPrefixes prefix blanks (stList t)

stPairList :: Show a => [(a,Tree a)] -> [String]
stPairList [] = ["leaf"]
stPairList (p:ps) = stPair "branch [ " p ++ concat (map (stPair "       , ") ps) ++ ["       ]"]

stList :: Show a => Tree a -> [String]
stList = stPairList . unwrapMS

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . showTree

leaf :: Tree a
leaf = MCons []

node :: [(a,Tree a)] -> Tree a
node l = MCons l

branch :: [Tree a] -> Tree a
branch = foldl (<|>) empty

-- Depth-first traversal
dfLabels :: Tree a -> [a]
dfLabels (MCons l) =  concat (Prelude.map (\(a,l') -> a:dfLabels l') l)

-- Breath-first traversal -- same as toList
bfLabels :: Tree a -> [a]
bfLabels t = bfLabs [t]

bfLabs :: [Tree a] -> [a]
bfLabs [] = []
bfLabs ((MCons l):ts) = Prelude.map fst l ++ bfLabs (ts ++ Prelude.map snd l)


t1 = node [(5, leaf),
           (9, node [(1, leaf)]),
           (2, node [(4, node [(3,leaf),(6,leaf)]),
                     (7,leaf)])
          ]

t2 = node [(10, node [(11,leaf)
                     ,(12,node [(13,leaf)])
                     ]
           )
          ,(20, leaf)
          ]
          

-- Tree that always branches according to a given list
--  Used for testing functions in Operations.hs
treeBranchList :: [a] -> Tree a
treeBranchList l = node $ map (\a -> (a,treeBranchList l)) l

-- Test a couple of the operations with:
--   printTree $ pruneMMS 3 $ prefixesMMS $ treeBranchList [0,1,2]

-- Tests to verify the 1st monad law

ts1 = return 1 >>= (\n -> treeBranchList [n,n+1,n+2])

ts1' = (\n -> treeBranchList [n,n+1,n+2]) 1

-- Tests to verify the 3rd monad law - the two seem equivalent when sliced at height n

ts3 = (treeBranchList [0,1,2] >>= (\n -> treeBranchList [n,n+1,n+2])) >>= (\n -> treeBranchList [n,n*2,n*3])

ts3' = treeBranchList [0,1,2] >>= (\x -> (\n -> treeBranchList [n,n+1,n+2]) x >>= (\n -> treeBranchList [n,n*2,n*3]))


-- Notes
-- - ma :: MonStr ZipList a seems to satisfy the property pairA (headMS ma) (tailMS ma) == ma

-- Interactive Processes -- IO Monad
------------------------------------

type Process a = MonStr IO a

runVoidProcess :: Process () -> IO ()
runVoidProcess (MCons s) = do (a,s') <- s
                              runVoidProcess s'

-- Perhaps needed to abstract what is returned by runProcess to a higher level - this allows for 
--  many different kinds of processes to be created (i.e. ones returning a Maybe value) - cannot be stopped
runProcess :: (Monad m, Monoid (m a)) => Process a -> IO (m a)
runProcess (MCons s) = do
    (a,s') <- s
    as <- runProcess s'
    return (mappend (return a) as)

-- Runs the process using unsafeInterleaveIO - this allows each IO
--  operation to be run at any time, allowing for lazy evaluation
unsafeRunProcess :: (Monad m, Monoid (m a)) => Process a -> IO (m a)
unsafeRunProcess (MCons s) = do
    (a,s') <- s
    as <- unsafeInterleaveIO (unsafeRunProcess s')
    return (mappend (return a) as)
    
-- Stops a process when a given predicate is true
stopAtPred :: (Monad m, Monoid (m a)) => (a -> Bool) -> Process a -> IO (m a)
stopAtPred p (MCons s) = do
     (a, s') <- s
     as <- (if p a then (return mempty) else stopAtPred p s')
     return (mappend (return a) as)
     
-- This stops after the next input from the user
--  The process is stopped when the Int value in the monad is 0 
stopAtZero :: (Monad m, Monoid (m Int)) => Process Int -> IO (m Int)
stopAtZero = stopAtPred (\n -> n == 0)

-- A stream that adds up the inputs from the user
-- Prints the partial sums
sumProc :: Int -> Process Int
sumProc n = MCons $ do
    putStrLn ("sum so far: " ++ (show n))
    s <- getLine
    let n' = n + read s
    return (n', sumProc n')

-- Proof of concept of lazy process evaluation 
stopAtZero' :: Process Int -> IO [Int]
stopAtZero' s = do
     ns <- unsafeRunProcess s
     return $! Prelude.takeWhile (/= 0) ns

-- State Monad
--------------

type StatefulStream s a = MonStr (State s) a

compileST :: Monad m => MonStr (StateT s m) a -> s -> MonStr m a
compileST (MCons sts) s = MCons $ do ((a, sts'), s') <- runStateT sts s
                                     return (a, compileST sts' s')

runSStr :: StatefulStream s a -> s -> Stream a
runSStr (MCons sts) s = let ((a, sts'), s') = runState sts s
                       in a <: (runSStr sts' s')
                       
-- generates a stream flipping between 1 and 0
flipper :: StatefulStream Int Int
flipper = MCons (state (\n -> ((n, flipper), (n+1) `mod` 2)))

-- generates a stream counting between 0 and (n-1) on a loop
modCounter :: Int -> StatefulStream Int Int
modCounter n = MCons (state (\x -> ((x, modCounter n), (x+1) `mod` n)))

-- a stream that generates the fibonnacci numbers
fibGen :: StatefulStream (Int, Int) Int
fibGen = MCons (state (\(a, b) -> ((b, fibGen), (b, a + b))))

fib :: Stream Int
fib = runSStr fibGen (0, 1)


-- Testing for monad laws

c  = takeMMS 20 $ runMS (return 5 >>= modCounter) 0
c' = takeMMS 20 $ runMS (modCounter 5) 0



-- Experimenting with State Monster Matrices

runMS :: StatefulStream s a -> s -> Stream (s,a)
runMS (MCons sts) s = let ((a, sts'), s') = runState sts s
                      in (s,a) <: (runMS sts' s')

rowMM :: (Int,Int) -> StatefulStream Int (Int,Int)
rowMM (x,y) = MCons $ do
  s <- get
  put (s+1)
  return ((x,y),rowMM (x,y+1))
  -- (x,y) <: rowMM (x,y+1)

coordMM :: (Int,Int) -> MonMatrix (State Int) (Int,Int)
coordMM (x,y) = MCons $ do
  s <- get
  put (s+100)
  return (rowMM (x,y), coordMM (x+1,y))
  -- rowMM (x,y) <: coordMM (x+1,y)

-- Using the two definitions of join gives different state behaviour
jC  = takeMMS 20 $ runMS (joinMS (coordMM (0,0))) 0
jC' = takeMMS 20 $ runMS (joinMS' (coordMM (0,0))) 0




-- Experimenting with possible monadic streams which could be useful for FRP
----------------------------------------------------------------------------

-- This is essentially the same as a signal function from Yampa
-- A function that returns a value alongside a continuation
-- a -> (b, a -> (b, a -> (b, ...
type SignalFunc a b = MonStr ((->) a) b

step :: SignalFunc a b -> a -> (b, SignalFunc a b)
step (MCons f) a = f a

stepN :: Int -> SignalFunc a a -> a -> (a, SignalFunc a a)
stepN 0 s a = step s a
stepN n (MCons f) a = let (a', s) = f a in stepN (n - 1) s a'

signal :: SignalFunc String Int
signal = MCons $ \n -> ((read n :: Int), signal)


-- Proof of concept of integration using the rectangle rule - works very similarly to the same function in Yampa

type DTime = Double

type ContSignalFunc a b = MonStr ((->) (DTime, a)) b

integral :: ContSignalFunc Double Double
integral = MCons integralAuxF
                 
integralAuxF :: (DTime, Double) -> (Double, ContSignalFunc Double Double)
integralAuxF (_, a) = (0 , integralAux 0 a)
                       where integralAux igrl a_prev = MCons (\(dt, a') -> (igrl' dt, integralAux (igrl' dt) a'))
                                                       where igrl' dt' = igrl + (dt' * a_prev)

evaluateCSF :: [(DTime,Double)] -> ContSignalFunc Double Double -> Double
evaluateCSF []     _   = error "Attempting to evaluate empty list of values"
evaluateCSF [x]    csf = (fst $ (unwrapMS csf) x)
evaluateCSF (x:xs) csf = evaluateCSF xs (snd $ (unwrapMS csf) x)

-- Basically an infinitely nested tuple (a, (b, (a, (b, (a, (b, ...
type ProductStream a b = MonStr ((,) a) b

{-
instance Monoid m => Comonad ((->) m) where
  duplicate f = \m -> f . mappend m
  extract f = f mempty

instance Comonad ((,) e) where
  duplicate p = (fst p, p)
  extract = snd
-}

newtype State' s a = State' {runState' :: s -> (a, s)}

instance Functor (State' s) where
  fmap f sf = State' $ \s -> let (a,s') = runState' sf s in (f a, s')

-- http://comonad.com/reader/2018/the-state-comonad/
-- If the State functor is over a monoid, then it forms a comonad
instance Monoid s => Comonad (State' s) where
  extract m = fst $ runState' m mempty
  
  duplicate m = State' $ \s ->
    ( State' (\t -> runState' m (mappend s t))
    , snd $ runState' m s
    )

type CoStatefulStream s a = MonStr (State' s) a

runCSStr :: CoStatefulStream s a -> s -> Stream a
runCSStr (MCons sts) s = let ((a, sts'), s') = runState' sts s
                         in a <: (runCSStr sts' s')
                       
oneStrCMS :: CoStatefulStream [Int] [Int]
oneStrCMS = MCons (State' (\s -> ((s, oneStrCMS), s ++ [1])))
