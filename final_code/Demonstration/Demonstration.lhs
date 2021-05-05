> module Demonstration.Demonstration where

> import Demonstration.DemonstrationHelper

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


> printLines :: Show a => [a] -> IO ()
> printLines = putStrLn . unlines . fmap show

---------------------
Branch-labelled trees
---------------------

> ballProbTree = genCondProbTree [LN ("Blue",   (1,7)), 
>                                 LN ("Red",    (2,7)),
>                                 LN ("Yellow", (1,7)),
>                                 LN ("Purple", (3,7))]

Using < takeM n > or < takeM' n > gives you all possible lists of choices of length n.
This can be used to find the proability of each outcome, given n choices.

> probOfEachN n = printLines $ map (foldr1 (*)) (takeM' (n + 1) ballProbTree)

Or, you can use < scanM > to give the whole tree of outcomes given n choices, which
is much neater and idomatic

> probOfEachN' n = printLines $ (scanM (*) ballProbTree) !! n

Using < !! n > you can see the list of all possible outcomes after taking n choices.
Collecting like terms here allows you to calculate probabilites of taking particular
colours on your next choice, after n previous choices.

< dropWithM (*) n > is also useful here - it allows calculation of all probabilities
past a particular number of choices

> probsAfter n = printLines $ collectLikeTerms . head $ dropWithM (*|) n ballProbTree

-----

With < dropWithM > you can verify that the calculated probabilities always add up to 1 at each 
level in the tree

> verifyProbs n = putStrLn . show $ foldr1 (+) (head (dropWithM (*) n ballProbTree))

-----

Given the tree of sequential probabilities, i.e. where the probabilities on the second layer have
been multiplied by the first:

> combinedProbs = scanM (*|) ballProbTree

you can use Bayes rule to recalculate the original tree!

> ballProbTree' = bayesScanTree combinedProbs

and then use equality on monadic streams to verify that these are indeed the same

> areTheyTheSame = putStrLn $ if ballProbTree == ballProbTree' then "Yes!" else "No!"


Sentence tree demo
------------------

You can also represent sentence trees, where the labels are characters and paths down
the tree are sentences. This shows how the words and unwords functions work, which are
both dependant on the choice of monad when it comes to their outputs, as shown.

> wordTreeDemo1 = wordsMFL wordTree

> wordTreeDemo2 = unwordsMFL wordTreeDemo1


--------------
State machines					
--------------

These two edge detectors are extensionally identical - looking at their defining code you
can see the similarity between Mealy machines and Reader-monsters

> edgesDemo = printLines . fst $ runSMStrList edgeDetector [O, I, O, O, I, I, I, O, O]

> edgesMealyDemo = printLines . fst $ runMealyList edgeDetectorMealy [O, I, O, O, I, I, I, O, O]


> trafficDemo = printLines . fst $ runSMStrList trafficLights [NS, Both, EW, EW, Both, Both, NS]


Insert demo
-----------

You can replace the (n+1)th action in any stream using < insert n >. The potential uses of
this are interesting considering each type of monadic stream

With state machines, you can stall the control flow, forcing it to stay in the same 
state for an extra tick regardless of input, providing a custom output during this
time.

> edgesInsertDemo = printLines . fst $ runSMStrList (insert 0 (\b -> "Stalled") edgeDetector) 
>                    					[O, I, O, O, I, I, I, O, O]

> trafficInsertDemo = printLines . fst $ runSMStrList (insert 0 (\_ -> (NS_Red, EW_Red)) trafficLights) 
>                      						[NS, Both, EW, EW, Both, Both, NS]


Zip demo
--------

> maxDemo = printLines $ zip (fst $ runSMStrList maxFin3 [One, Thr, Two, One, Thr, One, One, One, Thr, Two, Two, Two]) 
>                                        [One, Thr, Two, One, Thr, One, One, One, Thr, Two, Two, Two]

> minDemo = printLines $ zip (fst $ runSMStrList minFin3 [One, Thr, Two, One, Thr, One, One, One, Thr, Two, Two, Two]) 
>                                        [One, Thr, Two, One, Thr, One, One, One, Thr, Two, Two, Two]

> zipDemo = printLines $ zip (fst $ runSMStrList (zipWithA (==) maxFin3 minFin3) [One, Thr, Two, One, Thr, One, One, One, Thr, Two, Two, Two])
>                                        [One, Thr, Two, One, Thr, One, One, One, Thr, Two, Two, Two]

Processes
---------

See GameOfLife.hs
