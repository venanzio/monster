module Examples.Processes where
  
import MonadicStreams hiding ((++))
import System.IO.Unsafe

-- | Type of processes
type Process a = MonStr IO a

-- | Runs a process, throwing away the values. Will not terminate
runVoidProcess :: Process a -> IO ()
runVoidProcess (MCons s) = do (a,s') <- s
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

-- | A process that adds up the inputs from the user, printing
-- the partial sums
sumProc :: Int -> Process Int
sumProc n = MCons $ do
    putStrLn ("sum so far: " ++ (show n))
    s <- getLine
    let n' = n + read s
    return (n', sumProc n')

