-- Monadic Streams
--   Venanzio Capretta, 2020

import Stream
import Control.Monad.State
import System.IO.Unsafe
import Control.Concurrent
import Control.Concurrent.Async

-- Type of monadic streams
-- f is not required to be a monad

data MonStr m a = MCons (m (a , MonStr m a))

-- MonStr class instances and helper functions
----------------------------------------------


-- Example: Lazy lists - Maybe Monad
------------------------------------

type LList a = MonStr Maybe a

nil :: LList a
nil = MCons Nothing

cons :: a -> LList a -> LList a
cons a l = MCons (Just (a,l))

llist :: [a] -> LList a
llist = foldr cons nil

fromL :: LList a -> [a]
fromL (MCons Nothing) = []
fromL (MCons (Just (a,l))) = a : fromL l

-- Arbitrarily branching trees - List Monad
-------------------------------------------

type Tree a = MonStr [] a

leaf :: Tree a
leaf = MCons []

node :: [(a,Tree a)] -> Tree a
node l = MCons l

-- Depth-first traversal
dfLabels :: Tree a -> [a]
dfLabels (MCons l) =  concat (Prelude.map (\(a,l') -> a:dfLabels l') l)

-- Breath-first traversal
bfLabels :: Tree a -> [a]
bfLabels t = bfLabs [t]

bfLabs :: [Tree a] -> [a]
bfLabs [] = []
bfLabs ((MCons l):ts) = Prelude.map fst l ++ bfLabs (ts ++ Prelude.map snd l)


t1 = node [(5, leaf),
           (3, node [(1, leaf)]),
           (2, node [])
          ]

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
                       in a <:> (runStr sts' s')
                       
-- generates a stream flipping between 1 and 0
flipper :: StatefulStream Int Int
flipper = MCons (state (\n -> ((n, flipper), (n+1) `mod` 2)))

-- a stream that generates the fibonnacci numbers
fibGen :: StatefulStream (Int, Int) Int
fibGen = MCons (state (\(a, b) -> ((b, fibGen), (b, a + b))))

fib :: Stream Int
fib = runStr fibGen (0, 1)