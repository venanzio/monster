{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples.StateMachines where
  
import MonadicStream
import Examples.PureStreams
import Control.Monad.State
import Data.Fix

-- | Type of state machines, using the Reader monad
type SMStr i o = MonStr ((->) i) o

instance Show (SMStr i o) where
  show _ = "[ A Mealy machine ]"


-- | Correspondance between Reader-monsters and Mealy machines

-- | The type of Mealy machines
data Mealy st inA outA = Mealy { initState :: st
                               , transf :: (st , inA) -> (st , outA)
                               }

data StateFunc i o = SF { getSF :: i -> (StateFunc i o, o) }

type PreStateFunc i o = i -> (StateFunc i o, o)

-- | Every Mealy machine is a Reader-monster
mealyToMonStr :: Mealy s i o -> SMStr i o
mealyToMonStr (Mealy s tf) = MCons (\e -> let (s', a) = tf (s, e) in (a, mealyToMonStr (Mealy s' tf)))

-- | Every Reader-monster is a Mealy machine (with a state type of nested 
-- transition functions)
monStrToMealy :: SMStr i o -> Mealy (StateFunc i o) i o
monStrToMealy (MCons f) = Mealy (aux f) (\(g, e) -> (getSF g) e)
                          where 
                             aux :: (e -> (a, MonStr ((->) e) a)) -> StateFunc e a
                             aux f = SF (\e -> let (a, g) = f e in (aux (uncons g), a))



-- | Operations on and with state machines

-- | Building a Reader-monster out of function states
buildSMStr :: StateFunc i o -> SMStr i o
buildSMStr (SF s) = MCons $ \i -> let (s', o) = s i in (o, buildSMStr s')

-- | Notion of combining Mealy machines, using Reader-monsters
-- Basically just nested function composition
composeSM :: SMStr a b -> SMStr b c -> SMStr a c
composeSM (MCons f) (MCons g) = MCons $ \a -> let (b, f') = f a 
                                                  (c, g') = g b
                                                  in (c, composeSM f' g')

-- | Passes an input to the state machine, returning the output
-- and the next state
runSMStr :: SMStr i o -> i -> (o, SMStr i o)
runSMStr = uncons 

-- | Passes a list of inputs, one at a time, to the state machine,
-- collecting the outputs and returning the final state
runSMStrList :: SMStr i o -> [i] -> ([o], SMStr i o)
runSMStrList sm         []     = ([], sm)
runSMStrList (MCons sm) (i:is) = let (o, sm') = sm i in (\(os, smf) -> (o:os, smf)) $ runSMStrList sm' is
                          
                          
                                                   
-- | Example Mealy machines

{-
 | Traffic light example
 
 See https://tahull.github.io/projects/pic/traffic-light-fsm for
 the encoding of traffic states, as this example was adapted from
 there

 The traffic lights favour north-bound traffic over east-bound
-}

data TrafficInput = None | NS | EW | Both deriving Show

data NS_Lights = NS_Green | NS_Yellow | NS_Red deriving Show
data EW_Lights = EW_Green | EW_Yellow | EW_Red deriving Show

type TrafficOutput = (NS_Lights, EW_Lights)

s0 :: PreStateFunc TrafficInput TrafficOutput
s0 None = (SF s0, (NS_Green , EW_Red))
s0 NS   = (SF s0, (NS_Green , EW_Red))
s0 EW   = (SF s1, (NS_Yellow, EW_Red))
s0 Both = (SF s1, (NS_Yellow, EW_Red))

s1 :: PreStateFunc TrafficInput TrafficOutput
s1 _ = (SF s2, (NS_Red, EW_Green))
       
s2 :: PreStateFunc TrafficInput TrafficOutput
s2 None = (SF s3, (NS_Red, EW_Yellow))
s2 NS   = (SF s3, (NS_Red, EW_Yellow))
s2 EW   = (SF s2, (NS_Red, EW_Green ))
s2 Both = (SF s3, (NS_Red, EW_Yellow))

s3 :: PreStateFunc TrafficInput TrafficOutput
s3 _ = (SF s0, (NS_Green, EW_Red))

trafficLights :: SMStr TrafficInput TrafficOutput
trafficLights = buildSMStr (SF s0)


------------------------------------------------------------------------------

-- | Type of state machines with 'feedback loops'
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

