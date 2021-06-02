module Demonstration.GameOfLife where

import MonadicStreams hiding (iterate, iterate', repeat, (++), (!!), head, tail)
import qualified MonadicStreams as MS (repeat, iterate, iterate', (!!), head, tail)

import ComonadicStreams

import Control.Concurrent
import Examples.GenericStreams
import Examples.Processes 

import Demonstration.DemonstrationHelper

{-
 | This file contains a demonstration of Conway's game of
 life implemented using comonads and monadic streams.

 Most of the 'demo' code along with explanations is at the 
 bottom of the file.

 Comments prefixed with !!! indicate instructions to run the
 demonstrations given. 
 Beware: most of them don't terminate, and begin to slow down 
 quite quickly (after ~50 iterations) due to Haskells lazy
 evaluation - making evaluation of the Grid-monster stricter is
 a future goal.
-}

-- | The type of 2-dimensional grids with a focus on a
-- single cell, used as a stage for life
data Grid a = Grid ([([a],a,[a])], ([a],a,[a]), [([a],a,[a])])

-- | The type of infinitely nested 2-dimensional grids
type LifeStream a = MonStr Grid a
 
-- | Map grid tuple
mapGT :: (a -> b) -> ([a],a,[a]) -> ([b],b,[b])
mapGT f (ls, a, rs) = (map f ls, f a, map f rs)

triMap :: ([a] -> [b]) -> (a -> b) -> ([a] -> [b]) -> ([a],a,[a]) -> ([b],b,[b])
triMap lf af rf (ls, a, rs) = (lf ls, af a, rf rs)

-- | Operations to move the grid focus

leftShunt :: ([a],a,[a]) -> ([a],a,[a])
leftShunt (ls, a, (r:rs)) = ((a:ls), r, rs)

rightShunt :: ([a],a,[a]) -> ([a],a,[a])
rightShunt ((l:ls), a, rs) = (ls, l, (a:rs))

upRoll :: Grid a -> Grid a
upRoll (Grid t) = Grid (leftShunt t)

downRoll :: Grid a -> Grid a
downRoll (Grid t) = Grid (rightShunt t)

leftRoll :: Grid a -> Grid a
leftRoll (Grid t) = Grid (mapGT leftShunt t)

rightRoll :: Grid a -> Grid a
rightRoll (Grid t) = Grid (mapGT rightShunt t)

-- | Class instances for Grid 
 
instance Functor Grid where
  fmap f (Grid (lss, as, rss)) = Grid (map (mapGT f) lss, mapGT f as, map (mapGT f) rss)


omnidirectionalIteratedShunts :: Grid a -> Grid (Grid a)
omnidirectionalIteratedShunts g = Grid $ triMap (map lrShunts) lrShunts (map lrShunts) (tail (iterate downRoll g), g, tail (iterate upRoll g))
                                  where lrShunts g' = (tail (iterate rightRoll g'), g', tail (iterate leftRoll g'))

instance Comonad Grid where
  extract (Grid (_, (_,a,_), _)) = a
  duplicate = omnidirectionalIteratedShunts


-- | Game of life demonstration using the Grid comonad

countLine :: ([Bool], Bool, [Bool]) -> Int
countLine ((l:_), a, (r:_)) = sum [1 | b <- [l, a, r], b]

-- | The rule for evolving the grid
rule :: Grid Bool -> Bool
rule (Grid ((l:_), a@(_,f,_), (r:_))) = let n = (countLine l) + (if f then (countLine a) - 1 else countLine a) + (countLine r)
                                           in case n of
                                                 2 -> f
                                                 3 -> True
                                                 _ -> False

catTup :: ([a],a,[a]) -> [a]
catTup (ls,a,rs) = (reverse ls) ++ (a:rs)

gridToString :: Int -> Int -> Grid Bool -> String
gridToString w h (Grid (ups, as, dns)) = let ud = h `div` 2
                                             lr = w `div` 2
                                             cut = triMap (take lr) id (take lr)
                                             uplines = reverse (map (catTup . cut) (take ud ups))
                                             downlines = map (catTup . cut) (take ud dns)
                                             midline = catTup (cut as)
                                             printB b = if b then '#' else ' ' 
                                             in unlines (map (map printB) uplines) ++ (map printB midline) ++ "\n" ++ unlines (map (map printB) downlines)

{-
  #                         
   #                        
 ### 
-}   
                                            
glider1 :: Grid Bool
glider1 = Grid (
           (repeat False, True,  (repeat False)):(repeat emptyLine), 
           (repeat False, False, (True:(repeat False))),
           ((True:(repeat False)), True, (True:(repeat False))):(repeat emptyLine)
          )
          where emptyLine = (repeat False, False, repeat False)

{-
   #                      
  ###                     
 #                        
  ##                      
  #   
-}   
        
glider2 :: Grid Bool
glider2 = Grid (
           ((True:(repeat False)), True, (True:(repeat False))):(repeat False, True, (repeat False)):(repeat emptyLine), 
           (False:True:(repeat False), False, (False:(repeat False))),
           ((True:(repeat False)), True, (repeat False)):((True:(repeat False)), False, (repeat False)):(repeat emptyLine)
          )
          where emptyLine = (repeat False, False, repeat False)


-- | Functions to set cells to True or False before running with user input

set :: Bool -> Grid Bool -> Grid Bool
set a (Grid (lss, (ls, _, rs), rss)) = Grid (lss, (ls, a, rs), rss)

setCellX :: Bool -> Int -> Grid Bool -> Grid Bool
setCellX b x g | x == 0    = set b g
               | x > 0     = iterate rightRoll (set b (iterate leftRoll g !! x)) !! x
               | otherwise = iterate leftRoll (set b (iterate rightRoll g !! (abs x))) !! (abs x)

-- | Sets a cell relative to the current position
setCell :: Bool -> Int -> Int -> Grid Bool -> Grid Bool
setCell b x y g | y == 0    = setCellX b x g
                | y > 0     = iterate downRoll (setCellX b x (iterate upRoll g !! y)) !! y
                | otherwise = iterate upRoll (setCellX b x (iterate downRoll g !! (abs y))) !! (abs y)

newCell :: Grid Bool -> IO (Grid Bool)
newCell g = do putStrLn "Input x, y coord to turn on"
               x <- getLine >>= (readIO :: String -> IO Int)
               y <- getLine >>= (readIO :: String -> IO Int)
               return $ setCell True x y g


--------------------------------------------------
-- | RUNNING AND DEFINING THE FINAL PROCESSES | --
--------------------------------------------------

-- | A stream of GoL environments where each element is the string representation
lifeStreamShow :: LifeStream String
lifeStreamShow = evalMap (gridToString 50 50) (iterateC rule glider2)


-- | You can index a particular point in the stream of states with !@!
indexLife n = putStrLn $ extract (lifeStreamShow !@! n)


-- | This creates a process which prints the outputs at each point in the
-- life stream
lifeShowProc :: LifeStream String -> Process ()
lifeShowProc ls = MCons $ let (a, nxt) = extract (uncons ls)
                              in do putStrLn a
                                    continue (lifeShowProc nxt)

-- | This can be combined with the above process to add delays and
-- screen clearing between each iteration
--
-- !!! Run the proccess using: "runVoidProcess betterDisplayProc"
betterDisplayProc :: Process ()
betterDisplayProc = MS.repeat (do threadDelay 250000; putStr "\ESC[2J")

-- | This shows how they can be combined
demoProc = interleaveActM (lifeShowProc lifeStreamShow) betterDisplayProc


-- | Interactive demo
---------------------

-- | A stream of streams where the nth is the stream where you can set n+1 cells before
-- the GoL is run
--
-- Here we use the monadic stream iterate function, to produce a monster from iterating
-- a function from a -> m a
interactiveLifeStream :: Process (LifeStream Bool)
interactiveLifeStream = fmap (iterateC rule) (MS.iterate newCell glider1)

-- | Turns each of the inner streams into processes which prints the evolution after a
-- particular number of cell additions by a user
--
interactiveLifeStreamShow :: Process (Process ())
interactiveLifeStreamShow = fmap (lifeShowProc . evalMap (gridToString 50 50)) interactiveLifeStream

-- | This lets you take the nth inner process, collecting the 'outer' IO actions
-- of setting cells along the way
--
-- Setting cells (5, 4) (5, 5) (5, 6), with n as 2 gives interesting behaviour
--
-- !!! Run the process using: "runVoidProcess (interactiveDemo {number >= 0})"
interactiveDemo :: Int -> Process ()
interactiveDemo n = interleaveActM betterDisplayProc (takeInnerM n interactiveLifeStreamShow)

-- | This is the same as the above, but stops at the 20th iteration 
--
-- Uses zipA to zip together the process with a monad-polymorphic stream of
-- natural numbers
-- 
-- !!! Run using: "interactiveDemoStop {number >= 0}"
interactiveDemoStop :: Int -> IO ()
interactiveDemoStop n = do stopAtPred (\(a,b) -> b == 20) $ zipA (interactiveDemo n) nats
                           putStrLn "Finished!"
                           