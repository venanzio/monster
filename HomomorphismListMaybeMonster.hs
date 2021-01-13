{- 
   Functions to show that operations on lists 
   and maybe monsters produce isomorphic objects

   Venanzio Capretta & Christopher Purdy, 2020
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe
import Test.QuickCheck.All
import MonStreams
import Operations
import Control.Monad
import MonStrExamples
import Data.Foldable
import Combinators
import Data.List
import Data.Maybe

-- This instance creates very basic monadic streams, where each action is the return "do-nothing" action in that monad
--   For example a generated List-monster would look like: [x_0,[x_1,[x_2,[x_3,...,
--   and a generated Maybe-monster would look like: Just (x_0, Just (x_1, Just (x_2, Just (x_3, ...
--   where x_n is a random
instance (Monad m, Arbitrary a) => Arbitrary (MonStr m a) where
   arbitrary = fmap MCons $ promote (return (liftM2 (,) arbitrary arbitrary))
  
genMaybeMonStr :: Gen (MonStr Maybe Int)
genMaybeMonStr = do x <- chooseInt (1,1000)
                    fmap (insertAct x Nothing) arbitrary

instance Eq a => Eq (MonStr Maybe a) where
   (MCons (Just (h,t))) == (MCons (Just (h',t'))) = h == h' && t == t'
   (MCons Nothing)      == (MCons Nothing)        = True
   (MCons _)            == (MCons Nothing)        = True
   (MCons Nothing)      == (MCons _)              = True
   
instance Show a => Show (MonStr Maybe a) where
   show (MCons m) = show m
   
repeatIOAction :: Int -> IO () -> IO ()     -- Inputs: integer and IO. Outputs: IO 
repeatIOAction 0 _      = return ()         -- exit recursive loop here
repeatIOAction n action = do action         -- action to perform
                             repeatIOAction (n-1) action 

-- Tests for isomorphism between list and maybe monster, with llist and toList as the two morphisms between the types

prop_monStr :: Property
prop_monStr = forAll genMaybeMonStr $ (\mas -> mas === llist (toList mas))

prop_list :: Property
prop_list = forAll (listOf1 (chooseInt (-1000,1000))) $ (\ls -> ls === toList (llist ls))

-- Helper functions to generate lists and monsters for the tests below 

(>*<) :: Gen a -> Gen b -> Gen (a,b)
a >*< b = liftM2 (,) a b

gen2Lists :: Gen ([Int],[Int])
gen2Lists = listOf1 (chooseInt (-1000,1000)) >*< listOf1 (chooseInt (-1000,1000))

genList :: Gen ([Int])
genList = listOf1 (chooseInt (-1000,1000))

-- Generates a random list and Maybe-monster with the same values
genListMonStr :: Gen ([Int], MonStr Maybe Int)
genListMonStr = do l <- (listOf1 (chooseInt (-1000,1000)))
                   return (l, llist l)
                   
genPairListMonStr :: Gen ([(Int,Int)], MonStr Maybe (Int,Int))
genPairListMonStr = do l <- (listOf1 (arbitrary :: Gen (Int,Int)))
                       return (l, llist l)

-- Tests to compare functions in Operations to those in Data.List
--  !NOTE! Might want to take a second look at function/related tests where 
--         the list is wrapped in Just - could be better to map empty list
--         to Nothing instead of Just []
--         The correct interpretation is probably: 
--           - Return Nothing if the Data.List function throws an "empty list" exception
--           - Return Just [] if the Data.List function returns []
 
-- Also need to bear in mind useful distributions of testing data:
-- https://www.schoolofhaskell.com/user/pbv/an-introduction-to-quickcheck-testing
--  - split / span / *other things returning lists based on predicates* are likely to return 
--    very unbalanced lists a lot of the time, since the random predicates and delimeters are
--    unlikely to be specific to the lists in question.

-- PASSES
prop_inits :: Property
prop_inits = forAll genListMonStr $
                \(l, ms) -> toList (initsMMS ms) === inits l

-- !!! FAILS !!! - tailMMS (MCons Nothing) = Nothing, so tailsMMS recurses forever     
prop_tails :: Property
prop_tails = forAll genListMonStr $
                \(l, ms) -> fmap toList (tailsMMS ms) === tails l
                
-- PASSES
prop_tailsF :: Property
prop_tailsF = forAll genListMonStr $
                 \(l, ms) -> fmap toList (tailsF ms) === tails l

-- !!! FAILS !!! - takeMMS n (MCons Just (1, MCons Nothing)) where n > 0 gives Nothing, but take n [1] gives [1]
prop_take :: Property
prop_take = forAll (genListMonStr >*< chooseInt (0,1000)) $
               \((l, ms), n) -> takeMMS n ms === Just (take n l)
               
-- !!! FAILS !!! - same reason as prop_take
prop_take' :: Property
prop_take' = forAll (genListMonStr >*< chooseInt (0,1000)) $
                \((l, ms), n) -> takeMMS' n ms === Just (take n l)

-- !!! FAILS !!! - takeMMS'' n ms - always generates a list of n values, with the last ones being empty (Nothing/null) values if n is larger than the length of ms
prop_take'' :: Property
prop_take'' = forAll (genListMonStr >*< chooseInt (0,1000)) $
                 \((l, ms), n) -> takeMMS'' n ms === fmap Just (take n l)

-- PASSES
prop_takeF :: Property
prop_takeF = forAll (genListMonStr >*< chooseInt (0,1000)) $
                 \((l, ms), n) -> takeF n ms === fmap Just (take n l)

-- PASSES
prop_drop :: Property
prop_drop = forAll (genListMonStr >*< chooseInt (0,1000)) $
               \((l, ms), n) -> drop n l == toList (dropMMS n ms)
-- PASSES
prop_scan :: Property
prop_scan = forAll ((genListMonStr >*< chooseInt (-1000,1000)) >*< (arbitrary :: Gen(Fun (Int,Int) Int)) ) $ 
               \(((l,ms),n),(Fn2 f)) -> toList (scanMMS f n ms) === scanl f n l

-- PASSES
prop_scan1 :: Property
prop_scan1 = forAll (genListMonStr >*< (arbitrary :: Gen(Fun (Int,Int) Int)) ) $ 
                \((l,ms),(Fn2 f)) -> toList (scanMMS1 f ms) === scanl1 f l
 
-- PASSES              
prop_append :: Property
prop_append = forAll (genListMonStr >*< genListMonStr) $
                 \((l,ms),(l',ms')) -> toList (ms +++ ms') === l ++ l'
 
-- !!! FAILS !!! - fails when taking 1 item from a monster with 1 element which satisifes the predicate, spanMMS tries to put that elment inside the Nothing tail when returning it
prop_span :: Property
prop_span = forAll (genListMonStr >*< (arbitrary :: Gen(Fun Int Bool)) ) $ 
               \((l,ms),(Fn p)) -> fmap (\(a,b) -> (a,toList b)) (spanMMS p ms) === Just (span p l)
                
-- PASSES
prop_spanF :: Property
prop_spanF = forAll (genListMonStr >*< (arbitrary :: Gen(Fun Int Bool)) ) $ 
                \((l,ms),(Fn p)) -> fmap (\(a,b) -> (a,toList b)) (spanF p ms) === Just (span p l)
        
-- ! Not implemented ! - will definitely fail since it relies on prop_span passing
prop_break :: Property
prop_break = undefined

-- PASSES
prop_init :: Property
prop_init = forAll genListMonStr $ 
               \(l,ms) -> initMMS ms === fmap Just (init l)

-- PASSES
prop_init' :: Property
prop_init' = forAll genListMonStr $ 
                \(l,ms) -> initMMS' ms === Just (init l)

-- PASSES
prop_init'' :: Property
prop_init'' = forAll genListMonStr $ 
                 \(l,ms) -> toList (initMMS'' ms) === init l

-- PASSES
prop_length :: Property
prop_length = forAll genListMonStr $ 
                 \(l,ms) -> length ms === length l
                 
-- PASSES
prop_last :: Property
prop_last = forAll genListMonStr $ 
               \(l,ms) -> lastMMS ms === Just (last l)

-- PASSES
prop_zip :: Property
prop_zip = forAll (genListMonStr >*< genListMonStr) $ 
              \((l,ms),(l',ms')) -> toList (zipMMS ms ms') === zip l l'
        
-- PASSES
prop_unzip :: Property
prop_unzip = forAll genPairListMonStr $ 
                \(l,ms) -> (\(a,b) -> (toList a, toList b)) (unzipMMS ms) === unzip l

-- PASSES
prop_zipWith :: Property
prop_zipWith = forAll ((genListMonStr >*< genListMonStr) >*< (arbitrary :: Gen(Fun (Int,Int) Int))) $ 
                  \(((l,ms),(l',ms')),(Fn2 f)) -> toList (zipWithMMS f ms ms') === zipWith f l l'
            
-- PASSES
prop_filter :: Property
prop_filter = forAll (genListMonStr >*< (arbitrary :: Gen(Fun Int Bool)) ) $ 
                 \((l,ms),(Fn p)) -> toList (filterMMS p ms) === filter p l

-- PASSES
prop_map :: Property
prop_map = forAll (genListMonStr >*< (arbitrary :: Gen(Fun Int Int)) ) $ 
              \((l,ms),(Fn f)) -> toList (fmap f ms) === map f l

-- PASSES
prop_head :: Property
prop_head = forAll genListMonStr $
               \(l, ms) -> Just (head l) === headMS ms

-- PASSES
prop_tail :: Property
prop_tail = forAll genListMonStr $
               \(l, ms) -> Just (tail l) === fmap toList (tailMS ms)

-- PASSES
prop_tail' :: Property
prop_tail' = forAll genListMonStr $
                \(l, ms) -> tail l === toList (tailMMS ms)

-- PASSES
prop_index :: Property
prop_index = forAll (genListMonStr >*< chooseInt (0,1000)) $
                \((l, ms), n) -> let i = min n (length l - 1) in 
                                    Just (l !! i) === ms !!! i

-- !!! FAILS !!! - makes use of spanMMS so groupMMS is broken for the same reason
prop_group :: Property
prop_group = forAll genListMonStr $
                \(l, ms) -> group l === toList (groupMMS ms)
                 
-- PASSES
prop_groupF :: Property
prop_groupF = forAll genListMonStr $
                 \(l, ms) -> group l === toList (groupF ms)
                 
-- PASSES
prop_partition :: Property
prop_partition = forAll ( genListMonStr >*< (arbitrary :: Gen(Fun Int Bool)) ) $ 
                    \((l, ms),(Fn p)) -> Just (partition p l) === fmap (\(a,b) -> (toList a, toList b)) (partitionMMS p ms)

-- !!! FAILS !!! - when the index is larger than the list, splitAtMMS returns lots of 'Nothing's padding the end of the first list in the pair
prop_splitAt :: Property
prop_splitAt = forAll (genListMonStr >*< chooseInt (0,1000)) $
                  \((l, ms), n) -> ((\(a,b) -> (fmap Just a, llist b)) (splitAt n l)) === splitAtMMS n ms
           
-- PASSES
prop_splitAtF :: Property
prop_splitAtF = forAll (genListMonStr >*< chooseInt (0,1000)) $
                   \((l, ms), n) -> ((\(a,b) -> (fmap Just a, llist b)) (splitAt n l)) === splitAtF n ms

-- !!! FAILS !!! - makes use of spanMMS which is broken
prop_takeWhile :: Property
prop_takeWhile = forAll ( genListMonStr >*< (arbitrary :: Gen(Fun Int Bool)) ) $ 
                    \((l, ms),(Fn p)) -> Just (takeWhile p l) === takeWhileMMS p ms

-- PASSES
prop_takeWhileF :: Property
prop_takeWhileF = forAll ( genListMonStr >*< (arbitrary :: Gen(Fun Int Bool)) ) $ 
                     \((l, ms),(Fn p)) -> Just (takeWhile p l) === takeWhileF p ms

-- PASSES
prop_dropWhile :: Property
prop_dropWhile = forAll ( genListMonStr >*< (arbitrary :: Gen(Fun Int Bool)) ) $ 
                    \((l, ms),(Fn p)) -> dropWhile p l === toList (dropWhileMMS p ms)

-- PASSES
prop_intersperse :: Property
prop_intersperse = forAll ( genListMonStr >*< (arbitrary :: Gen(Int)) ) $
                       \((l, ms),n) -> intersperse n l === toList (intersperseMS (Just n) ms)
 
-- ! Not implemented ! - need to find a way to generate either a prefix or not (maybe generate a bool, then if true generate a random int below the length of the list and test with the prefix of the list of that length - if false generate a random list to test as prefix)
prop_isPrefixOf :: Property
prop_isPrefixOf = undefined

-- ! Not implemented !
prop_elemIndex :: Property
prop_elemIndex = undefined

-- ! Not implemented !
prop_elemIndicies :: Property
prop_elemIndicies = undefined

-- ! Not implemented !
prop_findIndex :: Property
prop_findIndex = undefined

-- ! Not implemented !
prop_findIndicies :: Property
prop_findIndicies = undefined

-- ! Not implemented ! - need to find a way to generate strings with a high chance of spaces to test this well
prop_words :: Property
prop_words = undefined

-- ! Not implemented ! - need to find a way to generate strings with a high chance of new lines to test this well
prop_lines :: Property
prop_lines = undefined

-- ! Not implemented ! - need to find a way to generate strings with a high chance of spaces to test this well
prop_unwords :: Property
prop_unwords = undefined

-- ! Not implemented ! - need to find a way to generate strings with a high chance of new lines to test this well
prop_unlines :: Property
prop_unlines = undefined

---- Runs all tests ----
return []
runTests :: IO Bool
runTests = $quickCheckAll