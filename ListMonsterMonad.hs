{-

 Investigation into whether list monsters are monads, and if not, 
  why, since finite-braching trees are

-}
import Test.QuickCheck
import Control.Monad
import MonStreams
import MonStrExamples

-- Definition of tree monad from https://dkalemis.wordpress.com/2014/03/22/trees-as-monads/

data Tree a = Node a [Tree a] deriving (Show, Eq)

instance Functor Tree where
   fmap f (Node x treeList) = Node (f x) (map (fmap f) treeList)
 
instance Applicative Tree where
   pure x = Node x []
   (Node f treeFunctionList) <*> (Node x treeElementList) =
      Node (f x) ( (map (fmap f) treeElementList) ++ (map (<*> (Node x treeElementList)) treeFunctionList) )

instance Monad Tree where
   return x = Node x []
   -- m a -> (a -> m b) -> m b
   Node x treeList >>= f = Node x' (treeList' ++ map (>>= f) treeList)
                           where Node x' treeList' = f x
                           
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    sized arbitrarySizedTree

arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree m = do
  t <- arbitrary
  n <- choose (0, m `div` 2)
  ts <- vectorOf n (arbitrarySizedTree (m `div` 4))
  return (Node t ts)

-- returns a list of slices at down to depth n
takeT :: Int -> Tree a -> [[a]]
takeT n (Node a ts) = [a]:(concat . map (takeT n) $ ts)
   
{-
   
  Monad laws:
  
     return >=> g = g
     
     f >=> return = f

     f >=> (g >=> h) = (f >=> g) >=> h

-}   

-- Example functions

factors :: Int -> Tree Int
factors n = Node n (map factors [x | x <- [1..(n-1)], n `mod` x == 0])

lessThan :: Int -> Tree Int
lessThan n = Node n (map lessThan [x | x <- [1..(n-1)]])

nextFive :: Int -> Tree Int
nextFive n = Node n (map (fmap (+5)) [Node x [] | x <- [n..(n+5)]])
   
   
-- QuickCheck properties to check if Tree is a monad - it seems to be
   
prop_left :: Property
prop_left = forAll arbitrary $ (\n -> (return >=> factors) n === factors n)

prop_right :: Property
prop_right = forAll arbitrary $ (\n -> (factors >=> return) n === factors n)

prop_assoc :: Property
prop_assoc = forAll (chooseInt (1,5)) $ (\n -> (factors >=> (nextFive >=> lessThan)) n === ((factors >=> nextFive) >=> lessThan) n)

{-

 data MonStr [] a = MCons [(a , MonStr [] a)]

   It might be a crucial difference that the root node of each subtree doesn't have a value in a List-monster?

 Monad instance for Tree works (for some element (Tree a ts)) by:

    - Applying f to a, returning (Tree a' ts')
    - Mapping (>>= f) across ts (the old subtree) and concatenating the resulting subtree with the new subtree ts'
    - ALL VALUES PRODUCED BY f ARE IN THE RESULTING TREE

 Tree a is the list instance of a Cofree type?

 Monad instance for monadic streams instanciated with lists works like this (for some element MCons [(a , ts)]):

    - f is applied to a and fmapped across ts, resulting in a monster matrix of lists (quite hard to visualise)
    - Each of the inner monsters get their tails dropped to some extent (with headMS)

-}

{-
joinPrelimMS :: Monad m => MonMatrix m a -> MonStr m (m a)
joinPrelimMS mm = MCons $ pure (\(as,ss) -> (headMS as, joinPrelimMS (fmap (absorbMS . tailMS) ss))) <*> unwrapMS mm

joinInnerMS :: Monad m => MonStr m (m a) -> MonStr m a
joinInnerMS mas = MCons $ join (pure (\(ma, ss) -> fmap (\a -> (a, joinInnerMS ss)) ma) <*> unwrapMS mas)

makeMonMatrix :: Monad m => (a -> MonStr m b) -> MonStr m a -> MonMatrix m b
makeMonMatrix f = fmap f

joinMS' ::  Monad m => MonMatrix m a -> MonStr m a
joinMS' = joinInnerMS . joinPrelimMS

outMS :: Functor m => MonStr m a -> (m a, m (MonStr m a))
outMS ms = (headMS ms, tailMS ms)

instance Monad m => Monad (MonStr m) where
  -- (>>=) :: MonStr m a -> (a -> MonStr m b) -> MonStr m b
  as >>= f = (joinInnerMS . joinPrelimMS . makeMonMatrix f) as
-}