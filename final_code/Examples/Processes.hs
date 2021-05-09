module Examples.Processes where
  
import Prelude hiding (head, tail)
import qualified Prelude as P (head, tail)
import MonadicStreams hiding ((++), (!!))
import System.IO.Unsafe
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Examples.GenericStreams

-- | Type of processes
type Process a = MonStr IO a

-- | Shorthand for returning nothing and continuing the process
continue :: Process a -> IO ((), Process a)
continue proc = return ((), proc)

-- | Runs a process, throwing away the values. Will not terminate
runVoidProcess :: Process a -> IO ()
runVoidProcess (MCons s) = do (a, s') <- s
                              runVoidProcess s'
                              
-- | Runs a process, accumulating the values. Will not terminate
runProcess :: Process a -> IO [a]
runProcess (MCons s) = do (a,s') <- s
                          as     <- runProcess s'
                          return (a:as)
                              
-- | Runs the process using unsafeInterleaveIO - this allows each IO
-- operation to be run at any time, allowing for lazy evaluation. This
-- function also will not terminate
unsafeRunProcess :: Process a -> IO [a]
unsafeRunProcess (MCons s) = do (a,s') <- s
                                as     <- unsafeInterleaveIO (unsafeRunProcess s')
                                return (a:as)
                             
-- | Runs a process, and stops it when a given predicate is true,
-- returning the value returned from the last IO computation
stopAtPred :: (a -> Bool) -> Process a -> IO a
stopAtPred p (MCons s) = do (a, s') <- s
                            if p a then (return a) else (stopAtPred p s')
                            
-- | Lazy process evaluation, will terminate when the predicate is false,
-- returning the list of accumulated values up to that point
stopAtPredLazy :: (a -> Bool) -> Process a -> IO [a]
stopAtPredLazy p s = do ns <- unsafeRunProcess s
                        return $! takeWhile (not . p) ns


-- | Examples of processes
--------------------------

-- | A process that adds up the inputs from the user, printing
-- the partial sums
sumProc :: Int -> Process Int
sumProc n = MCons $ do
    putStrLn ("sum so far: " ++ (show n))
    s <- getLine
    let n' = n + read s
    return (n', sumProc n')
    
    
-- | Example of benefits of processes as opposed to non-terminating
-- IO actions
consumeOne :: Show a => Process a -> Process a
consumeOne p = absorbM $ do (a, cont) <- uncons p
                            putStrLn (show a)
                            return cont

consumeOne' :: Show a => IO [a] -> IO (a, [a])
consumeOne' p = join $ do a <- fmap P.head p
                          putStrLn (show a)
                          return $ fmap (\l -> (a, P.tail l)) p

fromIO :: Int -> IO [Int]
fromIO n = (fmap (n:) (fromIO (n+1)))

-- | An example of combining two processes with interleaveReadM

inputProc :: Process Char
inputProc = MCons $ do c <- getChar
                       return (c, inputProc)
                       
-- | A neat way of printing outputs from an arbitrary process
-- This is like a 'dependent' process - each action needs to be
-- generated from an element of a
outputProc :: Show a => MonStr (ReaderT a IO) ()
outputProc = MCons $ do a <- ask 
                        liftIO $ putStrLn (show a)
                        return ((), outputProc)

-- | Showing how the input and output processes can be interleaved
testProc0 :: IO ()
testProc0 = runVoidProcess (interleaveReadM inputProc outputProc)



-- | A set of examples demonstrating the differences between lazy
-- and strict IO 

-- | Will not terminate or print the first element of the process
run0 :: IO ()
run0 = do as <- runProcess (sumProc 5)
          putStrLn $ show (as !! 0)

-- | Will terminate, printing the required element - uses lazy IO
run1 :: IO ()
run1 = do as <- unsafeRunProcess (sumProc 5)
          putStrLn $ show (as !! 0)

-- | Inserts the printing operation to execute it at the desired
-- point, but the process doesn't teerminate 
run2 :: IO ()
run2 = runProcess (insertActReadM 0 (\a -> do putStrLn (show a); return a) (sumProc 5)) >> return ()

-- | Does exactly the same as run1, but without unsafeInterleaveIO
run3 :: IO ()
run3 = stopAtPred (\(n, _) -> n == 0) (zipA nats proc) >> return ()
       where proc = insertActReadM 0 (\a -> do putStrLn (show a); return a) (sumProc 5)

-- | This also works fine now and is much cleaner
run4 :: IO ()
run4 = do a <- stopAtPred (\(n, _) -> n == 0) (zipA nats (sumProc 5)) 
          putStrLn $ show (snd a)