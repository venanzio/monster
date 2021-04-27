module Examples.StateMachines where
 
import Prelude hiding (head, tail, map, scanl, scanl1,
  iterate, take, drop, takeWhile, (++),
  dropWhile, repeat, cycle, filter, (!!), 
  zip, unzip, zipWith, zip3, unzip3, zipWith3,
  words,unwords,lines,unlines, break, span, splitAt)
  
import MonadicStream
import Examples.PureStreams
import Control.Monad.State
import Data.Fix

-- | Type of finite state machines
type FSMStr i o = MonStr ((->) i) o


-- | Correspondance of Reader-monsters with Mealy machines

-- | The type of Mealy machines
data Mealy st inA outA = Mealy { initState :: st
                               , transf :: (st , inA) -> (st , outA)
                               }

data MFunc e a b = MFunc {unFunc :: e -> (b , a)}

-- | Every Mealy machine is a Reader-monster
mealyToMonStr :: Mealy s i o -> FSMStr i o
mealyToMonStr (Mealy s tf) = MCons (\e -> let (s', a) = tf (s, e) in (a, mealyToMonStr (Mealy s' tf)))

-- | Every Reader-monster is a Mealy machine
monStrToMealy :: FSMStr i o -> Mealy (Fix (MFunc i o)) i o
monStrToMealy (MCons f) = Mealy (aux f) (\(g, e) -> (unFunc (unFix g)) e)
                          where 
                             aux :: (e -> (a, MonStr ((->) e) a)) -> Fix (MFunc e a)
                             aux f = Fix (MFunc (\e -> let (a, g) = f e in (aux (uncons g), a)))

{-
Mealy (Fix (MFunc i o)) i o ~= Mealy (MonStr ((->) i) o) i o

but it seems more informative to see the type of states of the Mealy machine
as the type \nu x. e -> (x, a), as it can be conceptually separated from the 
generality of monadic streams.
-}
                            
-- | Notion of combining Mealy machines, using Reader-monsters
-- Basically just nested function composition
composeFSM :: FSMStr a b -> FSMStr b c -> FSMStr a c
composeFSM (MCons f) (MCons g) = MCons $ \a -> let (b, f') = f a 
                                                   (c, g') = g b
                                                   in (c, composeFSM f' g')
                                           


-- | Type of finite state machines with 'feedback loops'
type FBMachine i o = MonStr (State i) o

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

