module Demonstration.DemonstrationHelper where
 
import Examples.PureStreams 
import Examples.LazyLists
import Examples.BLTrees
import Examples.StateMachines
import Examples.Processes
import MonadicStreams

import Prelude hiding ((!!), (++), head, tail)
import qualified Prelude as P ((!!), (++))
import Data.List hiding ((!!), (++), head, tail, insert)
import qualified Data.List as DL (iterate)
import Control.Monad

clearLines :: Int -> IO ()
clearLines 0 = return ()
clearLines n = do putStrLn ""
                  clearLines (n - 1)

{-
 | This is an auxillary file for Demonstration.hs - it 
 isn't well documented, and is just to reduce clutter, 
 in order to focus on the demo rather than the code
 it consists of
-}

-- | Helper function to add together probabilities with the same label
collectLikeTerms :: Num a => [LabelledNum a] -> [LabelledNum a]
collectLikeTerms = foldr (\(LN (s, a)) acc -> case findIndex (\(LN (s', _)) -> s == s') acc of
                                                 Nothing -> (LN (s, a)):acc
                                                 Just i  -> let (ls, (LN (s', b)):rs) = splitAt i acc in 
                                                               ls P.++ (LN (s', a + b)):rs) []

labelled :: String -> (LabelledNum a -> Bool)
labelled s (LN (s', n)) = s == s'

relabel :: String -> LabelledNum a -> LabelledNum a
relabel s (LN (_, a)) = LN (s, a)


pagb :: String -> String -> BLTree (LabelledNum Fraction) -> LabelledNum Fraction
pagb s s' (MCons ms) = reduceFrac $ sum $ do (a, (MCons ms')) <- ms
                                             guard (labelled s' a)
                                             (b, _) <- ms'
                                             guard (labelled s b)
                                             return b
                            
pagbn :: String -> String -> BLTree (LabelledNum Fraction) -> LabelledNum Fraction
pagbn s s' (MCons ms) = reduceFrac $ sum $ do (a, (MCons ms')) <- ms
                                              guard (labelled s' a)
                                              (b, _) <- ms'
                                              guard (not (labelled s b))
                                              return b

divFrac :: LabelledNum Fraction -> LabelledNum Fraction -> LabelledNum Fraction
divFrac (LN (s, (a, b))) (LN (s', (c, d))) = LN ("(" P.++ s P.++ ")/(" P.++ s' P.++ ")", (a * d, b * c))


-- | Bayes rule final functions

-- | Prob of 1st given 2nd
bayes :: String -> String -> BLTree (LabelledNum Fraction) -> LabelledNum Fraction
bayes s s' ms = relabel s . reduceFrac $ divFrac (pagb s s' ms) ((pagb s s' ms) + (pagbn s s' ms))


bayesScanTree :: BLTree (LabelledNum Fraction) -> BLTree (LabelledNum Fraction)
bayesScanTree (MCons ms) = MCons $ do (a, ms') <- ms
                                      return (a, bayesScan a (MCons ms) ms')
                                  
 
bayesScan :: LabelledNum Fraction -> BLTree (LabelledNum Fraction) -> BLTree (LabelledNum Fraction) -> BLTree (LabelledNum Fraction)
bayesScan (LN (a, _)) (MCons ms) (MCons ms') = MCons $ do ((LN (b, n)), (MCons nxt)) <- ms'
                                                          if null nxt then return (bayes b a (MCons ms), MCons [])
                                                                      else return (bayes b a (MCons ms), bayesScan (LN (b, n)) (MCons ms') (MCons nxt))
                                  



