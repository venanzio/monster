{-
  Monadic Streams
  Instantiations with various monads
-}

module MonStrExamples where

import MonStreams
import PureStreams
import Operations

import Control.Applicative
import Control.Monad

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
natsLess10 :: MonStr Maybe Integer
natsLess10 = boundNat 0
  where boundNat n | n < 10    = MCons $ Just (n, boundNat (n+1))
                   | otherwise = MCons Nothing 

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


-- Arbitrarily branching trees - List Monad
-------------------------------------------

type Tree a = MonStr [] a

showTree :: Show a => Tree a -> String
showTree = stInd 0
  where stInd n (MCons lt) =
          concat $ map (("\n"++) . stIndPair n) lt
        stIndPair n (a,t) = (blanks n) ++ show a ++
                            stInd (n + length (show a)) t
        blanks n = take n (repeat ' ')

printTree :: Show a => Tree a -> IO ()
printTree = putStrLn . showTree

leaf :: Tree a
leaf = MCons []

node :: [(a,Tree a)] -> Tree a
node l = MCons l

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

-- Interactive Processes -- IO Monad
------------------------------------

type Process a = MonStr IO a

runProcess :: Process a -> IO [a]
runProcess (MCons s) = do
    (a,s') <- s
    as <- runProcess s'
    return (a:as)

-- Runs the process using unsafeInterleaveIO - this allows each IO
--  operation to be run at any time, allowing for lazy evaluation
unsafeRunProcess :: Process a -> IO [a]
unsafeRunProcess (MCons s) = do
    (a,s') <- s
    as <- unsafeInterleaveIO (unsafeRunProcess s')
    return (a:as)

-- A stream that adds up the inputs from the user
-- Prints the partial sums

sumProc :: Int -> Process Int
sumProc n = MCons $ do
  putStrLn ("sum so far: " ++ (show n))
  s <- getLine
  let n' = n + read s
  return (n, sumProc n')

-- How to use the (infinite) return of a process
--  stop when it is zero

-- This stops after the next input from the user
stopAtZero :: Process Int -> IO [Int]
stopAtZero (MCons s) = do
     (n, s') <- s
     ns <- (if (n == 0) then (pure []) else stopAtZero s')
     return (n:ns)

stopAtZero' :: Process Int -> IO [Int]
stopAtZero' s = do
     ns <- unsafeRunProcess s
     return $! Prelude.takeWhile (/= 0) ns

-- But stopAtZero (sumProc 0) doesn't stop when the sum reaches 0
--   s not lazily evaluated?

-- Trying to find a way to make the output wait before the program is
--  finished
-- main :: IO ()
-- main = do
--   ret <- concurrently (stopAtZero' $ sumProc 5) (return 1)
--   print (ret :: ([Int], Int))


-- State Monad
--------------

type StatefulStream s a = MonStr (State s) a

-- collapseStateStream :: StatefulStream s a -> State s a
-- collapseStateStream (MCons sts) s = do
--                                       let ((a, sts'), s') = runState sts s
--                                       sts >>= collapseStateStream sts' s'

runStr :: StatefulStream s a -> s -> Stream a
runStr (MCons sts) s = let ((a, sts'), s') = runState sts s
                       in a <: (runStr sts' s')
                       
-- generates a stream flipping between 1 and 0
flipper :: StatefulStream Int Int
flipper = MCons (state (\n -> ((n, flipper), (n+1) `mod` 2)))

-- a stream that generates the fibonnacci numbers
fibGen :: StatefulStream (Int, Int) Int
fibGen = MCons (state (\(a, b) -> ((b, fibGen), (b, a + b))))

fib :: Stream Int
fib = runStr fibGen (0, 1)
