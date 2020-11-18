{- 
   Functions to show that operations on lists 
   and maybe monsters produce isomorphic objects

   Venanzio Capretta & Christopher Purdy, 2020
-}

{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe
import MonStreams
import Operations
import Control.Monad
import MonStrExamples
import Data.Foldable
import Combinators
import Data.List
import Data.Maybe

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

-- Tests for isomorphism between list and maybe monster, with llist and toList as the two morphisms between the types

propMonStr :: Property
propMonStr = forAll genMaybeMonStr $ (\mas -> mas === llist (toList mas))

propList :: Property
propList = forAll (listOf1 (chooseInt (-1000,1000))) $ (\ls -> ls === toList (llist ls))

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

-- Tests to compare functions in Operations to those in Data.List

prop_inits :: Property
prop_inits = forAll genListMonStr $
                (\(l, ms) -> toList (initsMMS ms) === inits l)

-- !!!FAILS!!! - tailMMS (MCons Nothing) = Nothing, so tailsMMS recurses forever     
prop_tails :: Property
prop_tails = forAll genListMonStr $
                (\(l, ms) -> fmap toList (tailsMMS ms) === tails l)

-- !!! FAILS !!! - takeMMS n (MCons Just (1, MCons Nothing)) where n > 0 gives Nothing, but take n [1] gives [1]
prop_take :: Property
prop_take = forAll (genListMonStr >*< chooseInt (0,1000)) $
               (\((l, ms), n) -> takeMMS n ms === Just (take n l))
                 