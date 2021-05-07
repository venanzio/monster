{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples.StateMachines where
  
import MonadicStreams
import Examples.PureStreams
import Control.Monad.State.Lazy

import Prelude hiding (head, tail)

-- | Type of state machines, using the Reader monad
type SMStr i o = MonStr ((->) i) o

-- | Convenient Show instance for masking Reader-monsters when
-- printed on the command line
instance Show (SMStr i o) where
  show _ = "[ A Mealy machine ]"
  
-- | Passes an input to the state machine, returning the output
-- and the next state
runSMStr :: SMStr i o -> i -> (o, SMStr i o)
runSMStr = uncons

-- | Passes a list of inputs, one at a time, to the state machine,
-- collecting the outputs and returning the final state
runSMStrList :: SMStr i o -> [i] -> ([o], SMStr i o)
runSMStrList sm         []     = ([], sm)
runSMStrList (MCons sm) (i:is) = let (o, sm') = sm i in (\(os, smf) -> (o:os, smf)) $ runSMStrList sm' is


-- | The usual type of Mealy machines
data Mealy st inA outA = Mealy { initState :: st
                               , transf :: (st , inA) -> (st , outA)
                               }
                               
runMealy :: Mealy s i o -> i -> (o, Mealy s i o)
runMealy (Mealy s0 tf) i = let (s, o) = tf (s0, i) in (o, Mealy s tf)

runMealyList :: Mealy s i o -> [i] -> ([o], Mealy s i o)
runMealyList (Mealy s0 tf) []     = ([], Mealy s0 tf)
runMealyList (Mealy s0 tf) (i:is) = let (s, o) = tf (s0, i) in (\(os, smf) -> (o:os, smf)) $ runMealyList (Mealy s tf) is

-- | Correspondance between Reader-monsters and Mealy machines

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
           
                                                   
-- | Example Mealy machines

-- | Edge detector example

data Bin = O | I deriving Show

es0 :: PreStateFunc Bin String
es0 O = (SF es1, "No edge")
es0 I = (SF es2, "No edge")

es1 :: PreStateFunc Bin String
es1 O = (SF es1, "No edge")
es1 I = (SF es2, "Edge detected")

es2 :: PreStateFunc Bin String
es2 O = (SF es1, "Edge detected")
es2 I = (SF es2, "No edge")

edgeDetector :: SMStr Bin String
edgeDetector = buildSMStr (SF es0)


-- | The actual Mealy machine for comparison

data EDState = A | B | C deriving Show

edMTrans :: (EDState, Bin) -> (EDState, String)
edMTrans (A, O) = (B, "No edge")
edMTrans (A, I) = (C, "No edge")
edMTrans (B, O) = (B, "No edge")
edMTrans (B, I) = (C, "Edge detected")
edMTrans (C, O) = (B, "Edge detected")
edMTrans (C, I) = (C, "No edge")

edgeDetectorMealy :: Mealy EDState Bin String
edgeDetectorMealy = Mealy A edMTrans

{-
 | Traffic light example
 
 See https://tahull.github.io/projects/pic/traffic-light-fsm for
 the encoding of traffic states, as this example was adapted from
 there

 The traffic lights favour north-bound traffic over east-bound, in
 presence of no traffic will turn NS lights green.
-}

data TrafficInput = None | NS | EW | Both deriving (Show, Eq)

data NS_Lights = NS_Green | NS_Yellow | NS_Red deriving (Show, Eq)
data EW_Lights = EW_Green | EW_Yellow | EW_Red deriving (Show, Eq)

type TrafficOutput = (NS_Lights, EW_Lights)

ts0 :: PreStateFunc TrafficInput TrafficOutput
ts0 None = (SF ts0, (NS_Green , EW_Red))
ts0 NS   = (SF ts0, (NS_Green , EW_Red))
ts0 EW   = (SF ts1, (NS_Yellow, EW_Red))
ts0 Both = (SF ts1, (NS_Yellow, EW_Red))

ts1 :: PreStateFunc TrafficInput TrafficOutput
ts1 _ = (SF ts2, (NS_Red, EW_Green))
       
ts2 :: PreStateFunc TrafficInput TrafficOutput
ts2 None = (SF ts3, (NS_Red, EW_Yellow))
ts2 NS   = (SF ts3, (NS_Red, EW_Yellow))
ts2 EW   = (SF ts2, (NS_Red, EW_Green ))
ts2 Both = (SF ts3, (NS_Red, EW_Yellow))

ts3 :: PreStateFunc TrafficInput TrafficOutput
ts3 _ = (SF ts0, (NS_Green, EW_Red))

trafficLights :: SMStr TrafficInput TrafficOutput
trafficLights = buildSMStr (SF ts0)


{-
 | Max and min example to demonstrate composition with
 zipWithA

-}

data Fin3 = One | Two | Thr deriving (Show, Eq, Ord)

-- | Max of last 3 inputs

-- Previous input was 3
mxb3 :: PreStateFunc Fin3 Fin3
mxb3 One = (SF mx3b1, Thr)
mxb3 Two = (SF mx3b2, Thr)
mxb3 Thr = (SF mxb3, Thr)

-- Last inputs were 3 then 2
mx3b2 :: PreStateFunc Fin3 Fin3
mx3b2 One = (SF mx2b1, Thr)
mx3b2 Two = (SF mxb2, Thr)
mx3b2 Thr = (SF mxb3, Thr)

-- Last input was 2
mxb2 :: PreStateFunc Fin3 Fin3
mxb2 One = (SF mx2b1, Two)
mxb2 Two = (SF mxb2, Two)
mxb2 Thr = (SF mxb3, Thr)

-- Last inputs were 2 then 1
mx2b1 :: PreStateFunc Fin3 Fin3
mx2b1 One = (SF mx1b1, Two)
mx2b1 Two = (SF mxb2, Two)
mx2b1 Thr = (SF mxb3, Thr)

-- Last inputs were 1 then 1
mx1b1 :: PreStateFunc Fin3 Fin3
mx1b1 One = (SF mx1b1, One)
mx1b1 Two = (SF mxb2, Two)
mx1b1 Thr = (SF mxb3, Thr)

-- Last inputs were 3 then 1
mx3b1 :: PreStateFunc Fin3 Fin3
mx3b1 One = (SF mx1b1, Thr)
mx3b1 Two = (SF mxb2, Thr)
mx3b1 Thr = (SF mxb3, Thr)

-- Entry state
mxa :: PreStateFunc Fin3 Fin3
mxa One = (SF mx1b1, One)
mxa Two = (SF mxb2, Two)
mxa Thr = (SF mxb3, Thr)

maxFin3 :: SMStr Fin3 Fin3
maxFin3 = buildSMStr (SF mxa)


-- | Min of last 3 inputs

-- Previous input was 1
mnb1 :: PreStateFunc Fin3 Fin3
mnb1 One = (SF mnb1, One)
mnb1 Two = (SF mn1b2, One)
mnb1 Thr = (SF mn1b3, One)

-- Last inputs were 1 then 2
mn1b2 :: PreStateFunc Fin3 Fin3
mn1b2 One = (SF mnb1, One)
mn1b2 Two = (SF mnb2, One)
mn1b2 Thr = (SF mn3b3, One)

-- Last input was 2
mnb2 :: PreStateFunc Fin3 Fin3
mnb2 One = (SF mnb1, One)
mnb2 Two = (SF mnb2, Two)
mnb2 Thr = (SF mn2b3, Two)

-- Last inputs were 2 then 3
mn2b3 :: PreStateFunc Fin3 Fin3
mn2b3 One = (SF mnb1, One)
mn2b3 Two = (SF mnb2, Two)
mn2b3 Thr = (SF mn3b3, Two)

-- Last inputs were 1 then 3
mn1b3 :: PreStateFunc Fin3 Fin3
mn1b3 One = (SF mnb1, One)
mn1b3 Two = (SF mnb2, One)
mn1b3 Thr = (SF mn3b3, One)

-- Last inputs were 3 then 3
mn3b3 :: PreStateFunc Fin3 Fin3
mn3b3 One = (SF mnb1, One)
mn3b3 Two = (SF mnb2, Two)
mn3b3 Thr = (SF mn3b3, Thr)

-- Entry state
mna :: PreStateFunc Fin3 Fin3
mna One = (SF mnb1, One)
mna Two = (SF mnb2, Two)
mna Thr = (SF mn3b3, Thr)

minFin3 :: SMStr Fin3 Fin3
minFin3 = buildSMStr (SF mna)


-- | Zipping the two togther

zipMinMaxFin3 :: SMStr Fin3 Bool
zipMinMaxFin3 = zipWithA (==) minFin3 maxFin3


-- | Simpler example(s)

fibCalc :: SMStr (Int, Int) Int
fibCalc = MCons $ \(n, m) -> (m, MCons $ (uncons fibCalc) . const (m, n+m))

------------------------------------------------------------------------

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

fromStep :: Int -> FBMachine Int Int
fromStep n = MCons (state (\x -> ((x, fromStep (n+1)), x+n)))

-- | Generates the fibonnacci numbers
fibGen :: FBMachine (Int, Int) Int
fibGen = MCons (state (\(a, b) -> ((b, fibGen), (b, a + b))))

fibS :: Stream Int
fibS = runFBStr fibGen (0, 1)

