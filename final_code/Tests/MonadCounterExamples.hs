{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module MonadCounterExamples where
 
import Test.QuickCheck
import Test.QuickCheck.All
 
import Prelude hiding (head, tail)
import qualified Prelude as P ((!!), iterate, head, tail, cycle) 
 
import MonadicStreams
import Examples.StateMachines
import Examples.GenericStreams
import Examples.PureStreams

{-
 | Tests to show that 3 different ways of defining the join
 operation on monadic streams lead to violations of monad laws
 in specific cases

 These cases may or may not imply that all other cases also don't
 satisfy monad laws, but this is yet to be shown
-}

-- | CASE 1 - takes the first element of each inner monster
joinC1 :: Monad m => MonStr m (MonStr m a) -> MonStr m a
joinC1 (MCons mma) = MCons $ do (mas, mma') <- mma
                                fmap (\a -> (a, joinC1 mma')) (head mas)

-- | CASE 2 - takes the first inner monster
joinC2 :: Monad m => MonStr m (MonStr m a) -> MonStr m a
joinC2 = absorbM . head 

-- | CASE 3 - takes the diagonal
joinC3 :: Monad m => MonStr m (MonStr m a) -> MonStr m a
joinC3 (MCons mma) = MCons $ do (mas, mma') <- mma
                                let (ma, ts) = (head mas, fmap tailM mma')
                                   in fmap (\a -> (a, joinC3 ts)) ma


-- | IMPORTANT: Change the case of join here (joinC1, joinC2, joinC3),
-- and run the tests, to demonstrate the cases of join that make each
-- example fail
instance Monad m => Monad (MonStr m) where
  ma >>= f = joinC3 (fmap f ma)
 
 
-- | QuickCheck tests to show monad laws don't hold in specific cases of
-- join. The tests only check the first 20 elements since this is enough
-- to show that each stream is not equivalent

prop_leftIden :: Property 
prop_leftIden = takeS 20 (runFBStr (return 1 >>= fromStep) 1) === takeS 20 (runFBStr (fromStep 1) 1)

prop_rightIden :: Property 
prop_rightIden = takeS 20 (fromN 0 >>= return) === takeS 20 (fromN 0)


---- Runs all tests ----
return []
runTests :: IO ()
runTests = do b <- $quickCheckAll
              putStrLn (if b then "All tests passed!" else "Some tests failed!")