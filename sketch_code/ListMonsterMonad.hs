{-# LANGUAGE RankNTypes #-}
{-

 Investigation into whether list monsters are monads, and if not, 
  why, since finite-braching trees are

-}
import Test.QuickCheck
import Data.Fix
import Control.Monad
import Control.Comonad
import MonStreams
import Control.Monad
import Operations
import qualified Data.List.NonEmpty as NE

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
 A cofree comonad over an alternative functor yeilds a monad 
  - see instance here http://hackage.haskell.org/package/free-3.4.1/docs/Control-Comonad-Cofree.html, repeated below


instance Alternative f => Monad (Cofree f) where
  return x = x :< empty
  (a :< m) >>= k = case k a of
                     b :< n -> b :< (n <|> fmap (>>= k) m)
 
 It's clear here that the fact that monadic streams guard the first value with the functor means that this approach 
 cannot work for defining the monad instance for monadic streams
                     
-}



{-

 Monadic streams with the non-empty list functor are identical to cofree over lists?

-}


data Tree' a = NodeL [(a , Tree' a)]
{-
headT :: Tree' a -> (a, Tree' a)
headT (NodeL l) = head l

instance Functor Tree' where
  fmap f (NodeL []) = NodeL []
  fmap f (NodeL ts) = NodeL $ map (\(x, ts') -> (f x, fmap f ts')) ts

instance Applicative Tree' where
  pure x = TCons [(x, TCons [])] 
  
  (NodeL [])           <*> (NodeL tas) = tas
  (NodeL ((f,tfs):fs)) <*> (NodeL tas) = case tas of 
                                            [] -> NodeL []
                                            (a, tas'):as -> NodeL (f a, ...)

instance Monad Tree' where
  t >>= f = tjoin (fmap f t)
            where tjoin (NodeL [])  = NodeL []
                  tjoin (NodeL tts) = NodeL $ map (\(x, tts') -> (fst (headT x), tjoin tts')) tts
-}

{-
isoTreePhi :: Tree a -> Tree' a
isoTreePhi (Node a ts) = NodeL [(a, map isoTree ts)]
-}

{-
data RoseTree a = Node a [RoseTree a]

data MonStrTree a = MNode [(a , MonStrTree a)]

-- Can't produce a RoseTree without an element, but you can with MonStrTrees
--  therefore they are definitly not isomorphic
isoTreePhi :: MonStrTree a -> RoseTree a
isoTreePhi (MNode []) = undefined
isoTreePhi (MNode [(a, ts)]) = ...
-}

data RoseTree a = RNode a [RoseTree a]

phi :: (a, MonStr [] a) -> RoseTree a
phi (a , MCons ts) = RNode a (fmap phi ts)

psi :: RoseTree a -> (a, MonStr [] a)
psi (RNode a ts) = (a , MCons (fmap psi ts))

-- Used to transform monadic stream into cofree below
data Freedom f a b = F a (f b)

-- This type is isomorphic to Cofree (seems fairly certain looking at the shape)
--  MCons (F a f((), MCons (F a f((), ...   ~=    a :< f (a :< f (a :< f (....
type CofreeMonStr f a = MonStr (Freedom f a) ()

data Cofree f a = a :< f (Cofree f a)

-- Isomorphism between specific case of monadic stream and cofree construction
cofreeIsoPhi :: Functor f => CofreeMonStr f a -> Cofree f a
cofreeIsoPhi (MCons (F a ft)) = a :< (fmap (\(_,t) -> cofreeIsoPhi t) ft)

cofreeIsoPsi :: Functor f => Cofree f a -> CofreeMonStr f a
cofreeIsoPsi (a :< ft) = MCons (F a (fmap (\x -> ((), cofreeIsoPsi x)) ft))

-- NonEmptyTree a = MCons ((a, NonEmptyTree a) :| [(a, NonEmptyTree a)])
type NonEmptyTree a = MonStr NE.NonEmpty a


joinCMS :: Comonad f => MonStr f (MonStr f a) -> MonStr f a
joinCMS mm = MCons $ fmap (\(as,ss) -> (extract as, joinCMS (fmap (\(MCons s) -> snd (extract s)) ss))) (unwrapMS mm)

{-
instance (Applicative f, Comonad f) => Monad (MonStr f) where
   s >>= f = joinCMS (fmap f s)
-}
{-
instance Comonad NonEmpty where
  extend f w@ ~(_ :| aas) = f w :| case aas of
      []     -> []
      (a:as) -> toList (extend f (a :| as))
  extract ~(a :| _) = a

instance Monad NonEmpty where
  ~(a :| as) >>= f = b :| (bs ++ bs')
    where b :| bs = f a
          bs' = as >>= toList . f
          toList ~(c :| cs) = c : cs
-}


happyTreeFunc :: Int -> NonEmptyTree Int
happyTreeFunc n = MCons ((n, happyTreeFunc (n*2)) NE.:| [(x, happyTreeFunc (x*2)) | x <- [1..n]])

sadTreeFunc :: Int -> NonEmptyTree Int
sadTreeFunc n = MCons ((n, sadTreeFunc (n `div` 2)) NE.:| [(x, sadTreeFunc (x `div` 2)) | x <- [1..n]])

steadyTreeFunc :: Int -> NonEmptyTree Int
steadyTreeFunc n = MCons ((n, steadyTreeFunc (n + 1)) NE.:| [(n + x, steadyTreeFunc (n + x + 1)) | x <- [1..n]])

tf1 = return >=> steadyTreeFunc
tf2 = happyTreeFunc >=> return

tf3a = happyTreeFunc >=> (sadTreeFunc >=> steadyTreeFunc)
tf3b = (happyTreeFunc >=> sadTreeFunc) >=> steadyTreeFunc


data Mealy st inA outA = Mealy { initState :: st
                               , transf :: (st , inA) -> (st , outA)
                               }
                               
instance Show s => Show (Mealy s b c) where
  show (Mealy s _) = "Mealy machine in state: " ++ show s
                             
data MFunc e a b = MFunc {unFunc :: e -> (b , a)}

instance Show (MFunc e a b) where
  show _ = "Mealy function"


mealyToMonStr :: Mealy s i o -> MonStr ((->) i) o
mealyToMonStr (Mealy s tf) = MCons (\e -> let (s', a) = tf (s, e) in (a, mealyToMonStr (Mealy s' tf)))

monStrToMealy :: MonStr ((->) e) a -> Mealy (Fix (MFunc e a)) e a
monStrToMealy (MCons f) = Mealy (aux f) (\(g, e) -> (unFunc (unFix g)) e)
                          where 
                             aux :: (e -> (a, MonStr ((->) e) a)) -> Fix (MFunc e a)
                             aux f = Fix (MFunc (\e -> let (a, g) = f e in (aux (unwrapMS g), a)))

runMealy :: Mealy s i o -> i -> (o, Mealy s i o)
runMealy (Mealy s f) i = let (s', o) = f (s, i) in (o, Mealy s' f)

runMealyStr :: MonStr ((->) e) a -> e -> (a, MonStr ((->) e) a)
runMealyStr (MCons f) e = f e

data Bin = B0 | B1 deriving Show
data Ter = T0 | T1 | T2 deriving Show

endsInZero :: Mealy Ter Bin Bin
endsInZero = Mealy T0 f
              where f (T0, B0) = (T1, B1)
                    f (T0, B1) = (T0, B0) 
                    f (T1, B0) = (T2, B1)
                    f (T1, B1) = (T0, B0)
                    f (T2, B0) = (T0, B1)
                    f (T2, B1) = (T0, B0)

testaux :: Mealy s i o -> MonStr ((->) i) o -> [i] -> (o, o)
testaux mm ms []     = error "Empty input list"
testaux mm ms [i]    = (fst $ runMealy mm i, fst $ runMealyStr ms i)
testaux mm ms (i:is) = testaux (snd $ runMealy mm i) (snd $ runMealyStr ms i) is

test :: Mealy s i o -> [i] -> (o, o)
test mm is = testaux mm (mealyToMonStr mm) is

test1 = test (monStrToMealy $ mealyToMonStr endsInZero) [B0, B1, B0, B1]

{-
 Set of states S corresponds to the continuation function type in reader-monster

 MonStr (Reader e) a
                              
 S = e -> (e -> e -> ...) the set of functions from e (input alphabet) possible 
                              
 The current state is a function
                              
 Input alphabet corresponds to type e of inputs to state functions
 Output alphabet corresponds to type a of return values
                              
 transition function is application
-}


-- Other representation of Mealy machines
data Mealy' st inA outA = Mealy' { initState' :: st
                                 , transF' :: (st , inA) -> st
                                 , outF' :: (st , inA) -> outA
                                 }

mealyToMonStr' :: Mealy' s i o -> MonStr ((->) i) o
mealyToMonStr' (Mealy' s tf outf) = mealyToMonStr $ Mealy s (\p -> (tf p, outf p))

monStrToMealy' :: MonStr ((->) e) a -> Mealy' (Fix (MFunc e a)) e a
monStrToMealy' ms = let (Mealy s tf) = monStrToMealy ms in Mealy' s (\p -> fst $ tf p) (\p -> snd $ tf p)

{-
                
Comonoadic stream of monadic streams

head (wma : MonStr w (MonStr m a)) = h : (ma : MonStr m a, wma : MonStr w (MonStr m a) )

an interaction law can then be used between m and w to traverse the stream ma

phi : (m a, w (MonStr m a)) -> (a , MonStr m a)                       
    
-}

{-

Streams polymorphic on the monad - a stream of game states, where instanciating
with lists gives the game tree, with IO gives a playable game etc.

Stream of game states - need a function a -> GameState, then monster can
produce as to select game states?


-}