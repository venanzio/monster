module Processes where
 
import Prelude hiding (head, tail, map, scanl, scanl1,
  iterate, take, drop, takeWhile, (++),
  dropWhile, repeat, cycle, filter, (!!), 
  zip, unzip, zipWith, zip3, unzip3, zipWith3,
  words,unwords,lines,unlines, break, span, splitAt)
  
import MonadicStream

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

