module Examples.StateMachines where
 
import Prelude hiding (head, tail, map, scanl, scanl1,
  iterate, take, drop, takeWhile, (++),
  dropWhile, repeat, cycle, filter, (!!), 
  zip, unzip, zipWith, zip3, unzip3, zipWith3,
  words,unwords,lines,unlines, break, span, splitAt)
  
import MonadicStream
import Examples.PureStreams
import Control.Monad.State

-- | Type of finite state machines
type FSMachine i o = MonStr ((->) i) o









-- | Type of finite state machines with 'feedback loops'
type FBMachine s a = MonStr (State s) a

compileST :: Monad m => MonStr (StateT s m) a -> s -> MonStr m a
compileST (MCons sts) s = MCons $ do ((a, sts'), s') <- runStateT sts s
                                     return (a, compileST sts' s')

runFBStr :: FBMachine s a -> s -> Stream a
runFBStr (MCons sts) s = let ((a, sts'), s') = runState sts s
                       in a <: (runFBStr sts' s')
                       
-- | Generates a stream flipping between 1 and 0
flipper :: FBMachine Int Int
flipper = MCons (state (\n -> ((n, flipper), (n+1) `mod` 2)))

-- | Generates a stream counting between 0 and (n-1) on a loop
modCounter :: Int -> FBMachine Int Int
modCounter n = MCons (state (\x -> ((x, modCounter n), (x+1) `mod` n)))

-- a stream that generates the fibonnacci numbers
fibGen :: FBMachine (Int, Int) Int
fibGen = MCons (state (\(a, b) -> ((b, fibGen), (b, a + b))))

fib :: Stream Int
fib = runFBStr fibGen (0, 1)

