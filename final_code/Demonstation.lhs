> module Demonstration where

> import Examples.PureStreams
> import Examples.LazyLists
> import Examples.BLTrees
> import Examples.StateMachines
> import Examples.Processes
> import MonadicStreams

> import Prelude hiding ((!!), (++), head, tail)
> import qualified Prelude as P ((!!), (++))
> import Data.List hiding ((!!), (++), head, tail, insert)
> import qualified Data.List as DL (iterate)

> printLines = putStrLn . unlines . fmap show

Branch-labelled trees
---------------------

> ballProbTree = genCondProbTree [LN ("Blue",   (1,7)), 
>                                 LN ("Red",    (2,7)),
>                                 LN ("Yellow", (1,7)),
>                                 LN ("Purple", (3,7))]

Using < takeM n > or < takeM' n > gives you all possible lists of choices of length n.
This can be used to find the proability of each outcome, given n choices.

> probOfEachN n = printLines $ map (foldr1 (*)) (takeM' n ballProbTree)


Using < pruneL n > you can see the tree of probabilities after taking n choices.

Using < !! n > you can see the list of all possible outcomes after taking n choices.
Collecting like terms here allows you to calculate probabilites of taking particular
colours on your next choice, after n previous choices.

< pruneWithM f n > is also very useful here - it allows calculation of all probabilities
past a particular number of choices

> collectLikeTerms = foldr (\(LN (s, a)) acc -> case findIndex (\(LN (s', _)) -> s == s') acc of
>                                                  Nothing -> (LN (s, a)):acc
>                                                  Just i  -> let (ls, (LN (s', b)):rs) = splitAt i acc in 
>                                                                ls P.++ (LN (s', a + b)):rs) []

> probsAfter n = printLines $ collectLikeTerms . head $ pruneWithM (*|) n ballProbTree


With < pruneWithM > you can verify that the calculated probabilities always add up to 1 at each 
level in the tree

> verifyProbs n = putStrLn . show $ foldr1 (+) (head (pruneWithM (*) n ballProbTree))



State machines					
--------------

> edgesDemo = runSMStrList edgeDetector [O, I, O, O, I, I, I, O, O]

> trafficDemo = runSMStrList trafficLights [NS, Both, EW, EW, Both, Both, NS]

Insert demo
-----------

You can replace the (n+1)th action in any stream using < insert n >. The potential uses of
this are interesting considering each type of monadic stream

With state machines, you can stall the control flow, forcing it to stay in the same 
state for an extra tick regardless of input, and providing a custom output during this
time.

> edgesInsertDemo = runSMStrList (insert 0 (\b -> "Stalled") edgeDetector) 
>                    [O, I, O, O, I, I, I, O, O]

> trafficInsertDemo = runSMStrList (insert 0 (\_ -> (NS_Red, EW_Red)) trafficLights) 
>                      [NS, Both, EW, EW, Both, Both, NS]

