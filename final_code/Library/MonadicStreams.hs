{-# LANGUAGE TypeFamilies, ExplicitForAll, Rank2Types #-}

{-|
Module      : MonadicStreams
Description : Core library for building and working with monadic streams
Copyright   : (c) Chris Purdy, 2021
                  Venanzio Capretta, 2021
License     : GPL-3 (NEEDS CHECKING)
Maintainer  : cp766@cam.ac.uk
Stability   : experimental (NEEDS CHECKING)
Portability : POSIX (NEEDS CHECKING)

== __Function naming conventions__

A function name may have one or more letters suffixed, indicating 
what kinds of underlying functors the function is designed to 
operate on:
 * A @=@ Applicative
 * M @=@ Monad
 * F @=@ Foldable
 * L @=@ Alternative

A lack of any letters indicates that the function works on regular
functors, and so can be used with any monadic (functorial?) stream.

The functions (!!) and (++) do not follow this pattern for style reasons:
 * (!!) works for m Monad
 * (++) works for m Alternative

== Disclaimer
Contrary to the name, not all monadic streams (monsters for short) are monads, 
or have monadic underlying functors. 
In fact we have only been able to show that monsters with Representable underlying
functors are themselves monads.
-}
module MonadicStreams
   (
   -- The type of monadic streams
     MonStr(..),
     
     module Control.Monad,
     module Data.Functor.Identity,
     
     -- NonEmpty(..),
          
     uncons,
     head,
     tail,
     tailM,
     tailMF,
     (<::),
     pairA,
     (<:),
     (<:::),
     (<<*>>),
     transform,
     transformA,
     
     repeat,
     repeatA,
     liftToStrM,
     absorbM,
     graftF,
     toMonStr,
     unfold,
     iterate,
     iterate',
     decomp,
     mapOutM,
     evalMap,
     scanA,
     scanM,
     (++),
     (!!),
     innerCons,
     prefixA,
     prefix,
     
     initMF,
     initMF',
     initMF'',
     initsA,
     
     tailsM,
     tailsMF,
     
     takeM,
     takeM',
     takeM'',
     takeMF,
     takeMF',
     takeInnerM,
     
     spanM,
     spanMF,
     breakM,
     breakMF,
     
     takeWhileM,
     takeWhileMF,
     dropWhileM,
     dropWhileMF,
     
     partitionMF,
     groupM,
     groupMF,
     isPrefixOfMF,
     joinWithM,
     indexWithM,
     pruneL,
     pruneByML,
     dropM,
     dropWithM,
     
     splitAtM,
     splitAtM',
     splitAtMF,
     splitAtML,
     
     intersperse,
     intersperseMF,
     interleave,
     interleaveActM,
     interleaveReadM,
     insertActM,
     insertActReadM,
     insert,
     insertRead,
     
     unconsMF,
     lastMF,
     lengthF,
     depthF,
     
     cycleA,
     cycle,
     cycle',
     
     filterM,

     zipWithA,
     zipWith3A,
     zipA,
     zip3A,
     unzipA,
     unzip3A,

     wordsMFL,
     unwordsMFL,
     linesMFL,
     unlinesMFL,

     findIndexM,
     elemIndexM,
     findIndicesM,
     elemIndicesM
   )
   where

import Prelude hiding (head, tail, map, scanl, scanl1,
  iterate, take, drop, takeWhile, (++),
  dropWhile, repeat, cycle, filter, (!!), 
  zip, unzip, zipWith, zip3, unzip3, zipWith3,
  words,unwords,lines,unlines, break, span, splitAt)
import qualified Prelude as P ((!!), (++), iterate, head, tail, cycle)
 
import Control.Applicative
import Control.Monad hiding (filterM)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.Functor.Identity

-- | Type of monadic streams, m is not required 
-- to be a monad - only a functor. 
-- Terminology note: "m-monster" or "m-stream" 
-- refers to a monadic stream under a functor
-- m; an element of MonStr m a
data MonStr m a = MCons (m (a , MonStr m a))

-- * Basic deconstructors and convenience functions for
-- appending elements to monsters etc.

uncons :: MonStr m a -> m (a, MonStr m a)
uncons (MCons m) = m  

head :: Functor m => MonStr m a -> m a
head = fmap fst . uncons

tail :: Functor m => MonStr m a -> m (MonStr m a)
tail = fmap snd . uncons

-- | Tail for m Monad - joins the monadic action, returning a tail
-- that isn't wrapped in the functor
tailM :: Monad m => MonStr m a -> MonStr m a
tailM = MCons . join . fmap uncons . tail

-- | Tail for m Monad and Foldable - deals with empty tails
tailMF :: (Monad m, Foldable m) => MonStr m a -> MonStr m a
tailMF ma = let t = tail ma
               in if null t then error "MonadicStreams.tailMF: empty monadic stream" 
                            else (MCons . join . fmap uncons) t
              
-- | Appending a pure element in front of a stream
-- This dupicates the functor, so beware
infixr 5 <|:
(<|:) :: Functor m => a -> MonStr m a -> MonStr m a
a <|: s = MCons $ fmap (\(_,s') -> (a, s)) $ uncons s
          
-- | Appending an m-element in front of a stream
infixr 5 <::
(<::) :: Functor m => m a -> MonStr m a -> MonStr m a
ma <:: s = MCons (fmap (\a -> (a,s)) ma)

-- | Generic pairing operation for Applicatives
pairA :: Applicative m => m a -> m b -> m (a,b)
pairA = liftA2 (,)

-- | Appends a pure element to the beginning of the given monadic stream
infixr 5 <:
(<:) :: Applicative m => a -> MonStr m a -> MonStr m a
a <: s = pure a <:: s

-- | Appending an m-element in front of a 'wrapped' monadic stream
infixr 5 <:::
(<:::) :: Applicative m => m a -> m (MonStr m a) -> MonStr m a
ma <::: ms = MCons (pairA ma ms)

-- | Double Applicative application convenience function
infix 5 <<*>>
(<<*>>) :: (Applicative m1, Applicative m2) =>
           m1 (m2 (a->b)) -> m1 (m2 a) -> m1 (m2 b)
(<<*>>) = liftA2 (<*>)


-- * Class instance declarations

-- | Transform a monster by mapping head and tail to new 
-- head and tail under m
transform :: Functor m => 
               (a -> MonStr m a -> (b, MonStr m b)) ->
               MonStr m a -> MonStr m b
transform f s = MCons $ fmap (\(h,t) -> f h t) (uncons s)

-- | Corecursion operator for monsters: h gives the head or the monster,
-- m is the action that generates the recursive calls/tail
mcorec :: Functor m => (x -> a) -> (x -> m x) -> x -> MonStr m a
mcorec h m x = MCons $ fmap (\x' -> (h x', mcorec h m x')) (m x)

instance Functor m => Functor (MonStr m) where
  fmap f = transform (\a s -> (f a, fmap f s))

-- | Version of transform for applicative monsters
transformA :: Applicative m =>
               (a -> MonStr m a -> b -> MonStr m b -> (c, MonStr m c)) ->
               MonStr m a -> MonStr m b -> MonStr m c
transformA f as bs = MCons $ (\(a,as') (b,bs') -> f a as' b bs')
                               <$> uncons as <*> uncons bs

instance Applicative m => Applicative (MonStr m) where
  pure a = a <: pure a
  (<*>) = transformA (\f fs a as -> (f a, fs <*> as))  
  

instance (Functor m, Foldable m) => Foldable (MonStr m) where
  foldMap f s = foldMap f (head s) `mappend`
                foldMap id (fmap (foldMap f) (tail s))


instance Alternative m => Alternative (MonStr m) where
  empty = MCons empty
  (MCons m1) <|> (MCons m2) = MCons (m1 <|> m2)


instance (Foldable m, Applicative m, Eq (m a), Eq (m Bool)) => Eq (MonStr m a) where
   ma == mb = (head ma == head mb) && (if null (tail ma) 
                                          then True
                                          else ((fmap (uncurry (==)) (pairA (tail ma) (tail mb))) == pure True))
                                          

-- * Lifting of natural tranformations to monsters

-- | Modifies the functors, but preserves the elements. Does not necessarily always
-- work on m, n Monad for example - the natural transformation given would need to
-- preserve the monad laws
liftNat :: (Functor m, Functor n) => (forall a. (m a -> n a)) -> MonStr m a -> MonStr n a
liftNat phi (MCons ma) = MCons $ phi (fmap (\(a, s) -> (a, liftNat phi s)) ma)

-- | Allow lifting of a monad transformer to monsters
liftMT :: (MonadTrans t, Monad m, Functor (t m)) => MonStr m a -> MonStr (t m) a
liftMT = liftNat lift


-- * Operations for m Functor

-- | Repeatedly nests a value wrapped in a container into itself
-- to form a trivial monadic stream
repeat :: Functor m => m a -> MonStr m a
repeat ma = MCons $ fmap (\a -> (a, repeat ma)) ma

-- | Unfolds a monadic stream from a seed value - a monadic stream co-iterator
unfold :: Functor m => (b -> (m a, b)) -> b -> MonStr m a
unfold f b = a <:: unfold f b'
             where (a, b') = f b
             
-- | If m is Foldable, we could define alternative by grafting
graftF :: (Functor m, Foldable m) => MonStr m a -> MonStr m a -> MonStr m a
graftF s1 s2 = if null s1 then s2
                          else transform (\h t -> (h, graftF t s2)) s1
             
-- | Produces an infinite sequence of repeated applications of f to an element
-- of a
iterate :: Functor m => (a -> m a) -> a -> MonStr m a
iterate f a = MCons $ fmap (\a -> (a, iterate f a)) (f a)

-- | Produces an infinite sequence of repeated applications of f to an element
-- of m a
iterate' :: Functor m => (m a -> m a) -> m a -> MonStr m a
iterate' f x = x <:: iterate' f (f x)

-- | Deconstructs a MonStr into its head and tail - the head
-- and tail cannot be recombined to produce the same monster
-- unless the monad satisfies certain properties
-- (namely idempotency)
decomp :: Functor m => MonStr m a -> (m a, m (MonStr m a))
decomp ms = (head ms, tail ms)

-- | Prefixes a given monadic stream with a given list of m values
prefix :: Functor m => [m a] -> MonStr m a -> MonStr m a
prefix xs s = foldr (<::) s xs

-- | Maps a function that 'evaluates' each action over the stream, keeping
-- the actions but replacing their values with those evaluated
evalMap :: Functor m => (m a -> b) -> MonStr m a -> MonStr m b
evalMap f (MCons ma) = let b = f (fmap fst ma) in MCons $ fmap (\(a, ma') -> (b, evalMap f ma')) ma

-- | Appending an element in front of each element in a monster containing lists
innerCons :: Functor m => a -> MonStr m [a] -> MonStr m [a]
innerCons a s = fmap (a:) s

-- | Intersperses elements of ms with ma - works better for infinite streams
intersperse :: Functor m => m a -> MonStr m a -> MonStr m a
intersperse ma = transform (\h t -> (h, ma <:: (intersperse ma t)))

-- | Interleaves the elements of two monadic streams
interleave :: Functor m => MonStr m a -> MonStr m a -> MonStr m a
interleave mas mbs = transform (\h t -> (h, interleave mbs t)) mas

-- | Inserts a given functor action and its return value at the specified 
-- index in the stream
insert :: Functor m => Int -> m a -> MonStr m a -> MonStr m a
insert 0 ma mas = MCons $ fmap (\(h,t) -> (h, MCons $ fmap (\a -> (a, t)) ma)) (uncons mas)
insert n ma mas = MCons $ fmap (\(h,t) -> (h, insert (n-1) ma t)) (uncons mas)

-- | Inserts a 'dependent' value into the stream at a given index, that
-- reads the value of the element before it
insertRead :: Functor m => Int -> (a -> m a) -> MonStr m a -> MonStr m a
insertRead 0 f mas = MCons $ fmap (\(h, t) -> (h, MCons $ fmap (\a -> (a, t)) (f h))) (uncons mas)
insertRead n f mas = MCons $ fmap (\(h,t) -> (h, insertRead (n-1) f t)) (uncons mas)
                  
-- | Gives the total numbers of elements in a monster
--
-- /Beware/: passing a monadic stream not containing a 'null' element 
-- will cause the function to diverge
lengthF :: (Functor m, Foldable m) => MonStr m a -> Int
lengthF = length

-- | Length of longest path - diverges if there is an infinite path
depthF :: (Functor m, Foldable m) => MonStr m a -> Int
depthF = maximumInt . fmap (\(h,t) -> depthF t + 1) . uncons
         where maximumInt t | null t = 0
                            | otherwise = maximum t
                            
-- | Cycles the contents of the given list of values inside a monster
cycle :: Functor m => [m a] -> MonStr m a
cycle mas = foldr (\ma s -> (ma <:: s)) (cycle mas) (P.cycle mas)

-- | A version of cycle which accepts m [a] instead of [m a]
cycle' :: Functor m => m [a] -> MonStr m a
cycle' mas = cycle (unseq (fmap P.cycle mas))
             where unseq ~mas' = fmap P.head mas' : unseq (fmap P.tail mas')
             
-- | "Internal" version of take: pruning the stream at depth n
pruneL :: (Functor m, Alternative m) => Int -> MonStr m a -> MonStr m a
pruneL n s
  | n == 0 = empty
  | n > 0  = transform (\h t -> (h, pruneL (n-1) t)) s
  | otherwise = error "MonadicStreams.pruneFL: negative argument."


-- * Operations for m Applicaitve

-- | Repeats an element and turns it into a monadic stream - requires
-- m Applicative
repeatA :: Applicative m => a -> MonStr m a
repeatA = pure

-- | Scan returns the stream of intermediate results, operates
-- in the same was as scanl in Data.List
scanA :: Applicative m => (a -> b -> a) -> a -> MonStr m b -> MonStr m a
scanA f z s = z <: scanH f z s
              where scanH f z s = MCons $ (\(a, s') -> (f z a, scanH f (f z a) s')) <$> uncons s

-- | Prefixes a given monadic stream with a given list of values
prefixA :: Applicative m => [a] -> MonStr m a -> MonStr m a
prefixA xs s = foldr (<:) s xs

-- | Accumulating the prefixes in each path of a monster:
-- Every entry contains the list of its predecessors - the 
-- empty list is appended, to make it equivalent to inits 
-- in Data.List
initsA :: Applicative m => MonStr m a -> MonStr m [a]
initsA s = [] <: initsA' s
           where initsA' = transform (\h t -> ([h], innerCons h (initsA' t)))
           
-- | Converts a foldable container to a monadic stream
toMonStr :: (Foldable t, Applicative m, Alternative m) => t a -> MonStr m a
toMonStr = foldr (<:) empty

-- | Injects an infinite repetition of a list into a monster
cycleA :: Applicative m => [a] -> MonStr m a
cycleA as = foldr (<:) (cycleA as) as

-- | Zips the two given monsters using the given ternary function
-- this performs the same as liftA2 - it seems at the moment but hasn't be verified formally
zipWithA :: Applicative m => (a -> b -> c) -> MonStr m a -> MonStr m b -> MonStr m c
zipWithA = liftA2

-- | Zipping with 3 streams, using the ternary function given
zipWith3A :: Applicative m => (a -> b -> c -> d) -> MonStr m a -> MonStr m b -> MonStr m c -> MonStr m d
zipWith3A f ma mb mc = f <$> ma <*> mb <*> mc

-- | Takes two monadic streams and returns a monster of pairs obtained
-- by pairing elements at the same index in the argument monsters
zipA :: Applicative m => MonStr m a -> MonStr m b -> MonStr m (a,b)
zipA = zipWithA (,)

-- | Zipping with 3 streams
zip3A :: Applicative m => MonStr m a -> MonStr m b -> MonStr m c -> MonStr m (a,b,c)
zip3A = zipWith3A (,,)

-- | Inverse operations of zipMMS and zip3MMS
unzipA :: Applicative m => MonStr m (a, b) -> (MonStr m a, MonStr m b)
unzipA ms = (fmap fst ms, fmap snd ms)

-- | Unzipping a stream of 3-tuples
unzip3A :: Applicative m => MonStr m (a, b, c) -> (MonStr m a, MonStr m b,  MonStr m c)
unzip3A ms = (fmap (\(a,_,_) -> a) ms, fmap (\(_,b,_) -> b) ms, fmap (\(_,_,c) -> c) ms)


-- * Operations for m Monad

-- | Lifts monadic actions from elements to the stream
liftToStrM :: Monad m => MonStr m (m a) -> m (MonStr m a)
liftToStrM ms = pure (\(ma,ms') -> ma <::: liftToStrM ms') <*> uncons ms

-- | Monadic monsters can "absorb" a monadic action
absorbM :: Monad m => m (MonStr m a) -> MonStr m a
absorbM ms = MCons . join $ fmap uncons ms

-- | Transform a monadic monster by operating on the "raw" head 
-- and tail
mapOutM :: Monad m => (a -> MonStr m a -> MonStr m b) ->
                       MonStr m a -> MonStr m b
mapOutM f s = absorbM $ fmap (uncurry f) (uncons s)

-- | A verison of scan without a starting value argument - works 
-- very nicely on List-monsters (brach-labelled trees). This operates
-- in the same way as scanl1 in Data.List
scanM :: Monad m => (a -> a -> a) -> MonStr m a -> MonStr m a
scanM f s = absorbM $ (\(a, s') -> scanA f a s') <$> uncons s

-- | Appending two monsters: the second is grafted when there is an empty action
infixr 5 ++
(++) :: Alternative m => MonStr m a -> MonStr m a -> MonStr m a
s1 ++ s2 = (head s1 <::: ((++) <$> tail s1 <*> pure s2)) <|> s2

infixl 9 !!
-- | Indexing operator with m Monad
(!!) :: Monad m => MonStr m a -> Int -> m a
s !! n = head $ (P.iterate tailM s) P.!! n

-- | Returns the elements of the monster, minus the last element
initMF :: (Monad m, Foldable m) => MonStr m a -> [m a]
initMF ms = if isEnd tl then [] else (head ms : initMF tl)
            where tl = tailM ms
                  isEnd ms' = null . uncons $ ms'
              
-- | Version of init where the returned list is inside a single monadic action
initMF' :: (Monad m, Foldable m) => MonStr m a -> m [a]
initMF' = sequence . initMF
              
-- | Version of init where the last element is removed from the given finite MonStr
initMF'' :: (Monad m, Foldable m) => MonStr m a -> MonStr m a
initMF'' = mapOutM $ \hd tl -> if null tl
                                  then tl
                                  else hd <: initMF'' tl

-- | Constructs a list containing all of the suffix monsters of
-- the argument - equivalent to tails in Stream.hs (when used on
-- Maybe-monsters)
tailsM :: Monad m => MonStr m a -> [MonStr m a]
tailsM mas = mas : (tailsM (tailM mas))

-- | tails with Foldable type constraint
tailsMF :: (Monad m, Foldable m) => MonStr m a -> [MonStr m a]
tailsMF mas = if null (uncons mas) then [mas] else mas : (tailsMF (tailMF mas))

-- | Returns the list of first n elements. In the case of 
-- List-monsters (trees): paths of length n (ignores 
-- shorter paths)
takeM :: Monad m => Int -> MonStr m a -> m [a]
takeM n s
  | n == 0 = return []
  | n > 0  = uncons s >>= \(h,t) -> fmap (h:) (takeM (n-1) t)
  | otherwise = error "MonadicStreams.takeM: negative argument."

-- | Simpler alternative to takeM: nth elements of the inits
takeM' :: Monad m => Int -> MonStr m a -> m [a]
takeM' n s = initsA s !! n

-- | Extracting the list of the first n heads (each inside the monad)
-- In case of trees: slices at depths less than n
takeM'' :: Monad m => Int -> MonStr m a -> [m a]
takeM'' n ms
  | n == 0    = []
  | n > 0     = head ms : (takeM'' (n - 1) (tailM ms))
  | otherwise = error "MonadicStreams.takeM'': negative argument."

-- | take with  m Foldable - handles finite streams correctly
takeMF :: (Monad m, Foldable m) => Int -> MonStr m a -> [m a]
takeMF n ms
  | null (uncons ms) = []
  | n >= 0    = takeM'' n ms
  | otherwise = error "MonadicStreams.takeMF: negative argument."
  
-- | take with  m Foldable - returns the list inside a single monadic action
takeMF' :: (Monad m, Foldable m) => Int -> MonStr m a -> m [a]
takeMF' n ms = sequence (takeMF n ms)

-- | Takes the nth inner stream from a stream of streams
takeInnerM :: Monad m => Int -> MonStr m (MonStr m a) -> MonStr m a
takeInnerM n = absorbM . (!! n)

-- | Returns the longest prefix of ma that satisfies p
-- together with the remainder of the monadic stream, 
-- with the prefix inside the monad m
--
-- /Beware/: this function may diverge if every element
-- of the given monadic stream satisfies the predicate,
-- and has undefined behaviour when the stream is finite
spanM :: Monad m => (a -> Bool) -> MonStr m a -> m ([a], MonStr m a)
spanM p mas = do (a, mas') <- (uncons mas)
                 if not (p a) then return ([], mas) else (fmap (\(l, t) -> (a:l, t)) (spanM p mas'))

-- | span with Foldable type constraint
spanMF :: (Monad m, Foldable m) => (a -> Bool) -> MonStr m a -> m ([a], MonStr m a)
spanMF p mas = if null ma then return ([], mas) 
                          else ma >>= \(h,t) -> if p h then let ret = spanMF p t in 
                                                                fmap (\(l, s) -> (h:l, s)) ret
                                                       else return ([], h <: t)
               where ma = uncons mas

-- | breakM p is equivalent to spanM (not . p)
--
-- /Beware/: this function may diverge for the same reason as spanM
breakM :: Monad m => (a -> Bool) -> MonStr m a -> m ([a], MonStr m a)
breakM p = spanM (not . p)

-- | break with Foldable type constraint (using spanMF)
breakMF :: (Monad m, Foldable m) => (a -> Bool) -> MonStr m a -> m ([a], MonStr m a)
breakMF p = spanMF (not . p)

-- | This may diverge for the same reason as spanM
takeWhileM :: Monad m => (a -> Bool) -> MonStr m a -> m [a]
takeWhileM p ma = fmap fst $ spanM p ma

-- | takeWhile with m Foldable,
-- this may diverge for the same reason as spanMF
takeWhileMF :: (Monad m, Foldable m) => (a -> Bool) -> MonStr m a -> m [a]
takeWhileMF p ma = fmap fst $ spanMF p ma

-- | This may diverge for the same reason as spanM
dropWhileM :: Monad m => (a -> Bool) -> MonStr m a -> MonStr m a
dropWhileM p ma = absorbM . (fmap snd) $ spanM p ma

-- | This may diverge for the same reason as spanMF
dropWhileMF :: (Monad m, Foldable m) => (a -> Bool) -> MonStr m a -> MonStr m a
dropWhileMF p ma = absorbM . (fmap snd) $ spanMF p ma

-- | Partitions the given monadic stream into a pair of monadic streams - one where every 
-- element satisfies the predicate p, and one with the remaining elements which do not
partitionMF :: (Monad m, Foldable m) => (a -> Bool) -> MonStr m a -> m (MonStr m a, MonStr m a)
partitionMF p ma = uncons ma >>= \(h,t) -> let ret = (if null t then pure (t, t) else partitionMF p t) in 
                                               if p h then (\p' -> fmap (\(t, f) -> (h <: t, f)) p') ret
                                                      else (\p' -> fmap (\(t, f) -> (t, h <: f)) p') ret

-- | Groups consecutive equal elements of a monadic monster into lists
-- 
-- /Beware/: this function has undefined behaviour on finite 
-- streams
groupM :: (Monad m, Eq a) => MonStr m a -> MonStr m [a]
groupM mas = MCons . join $ (\(h,t) -> fmap (\(h',t') -> (h:h', groupM t')) (spanM (== h) t)) <$> uncons mas

-- | Group with m Foldable
groupMF :: (Monad m, Eq a, Foldable m) => MonStr m a -> MonStr m [a]
groupMF mas = MCons . join $ (\(h,t) -> fmap (\(h',t') -> (h:h', groupMF t')) (spanMF (== h) t)) <$> uncons mas

-- | isPrefixOfMMS returns True if the first argument is a prefix of the second, 
-- and False otherwise. Will diverge if the monsters being compared are the same 
-- and are infinite
isPrefixOfMF :: (Monad m, Foldable m, Eq a) => MonStr m a -> MonStr m a -> m Bool
isPrefixOfMF ma mb = if null ma 
                        then return True 
                        else if null mb 
                                then return False
                                else join $ (\(h,t) (h',t') -> if h /= h' then return False else (if null t then return True else isPrefixOfMF t t')) <$> uncons ma <*> uncons mb

-- | Joins the first two monadic actions of a monster, using a
-- binary function to join the return values
joinWithM :: Monad m => (a -> a -> a) -> MonStr m a -> MonStr m a
joinWithM f (MCons ma) = MCons $ do (a , (MCons mb)) <- ma
                                    (b , mc)         <- mb
                                    return (f a b, mc)

-- | Combines the first n elements, using a binary operation to
-- collect them together
indexWithM :: Monad m => (a -> a -> a) -> Int -> MonStr m a -> m a
indexWithM f n ms = head (dropWithM f n ms)

-- | Prunes a stream, 'cutting off' paths where an element doesn't satisfy
-- the predicate
pruneByML :: (Monad m, Alternative m) => (a -> Bool) -> MonStr m a -> MonStr m a
pruneByML p (MCons ms) = MCons $ do (a, ms') <- ms 
                                    guard (p a)
                                    return (a, pruneByML p ms')

-- | Drops the first n elements of a monster, joining the first n monadic 
-- actions in the process
dropM :: Monad m => Int -> MonStr m a -> MonStr m a
dropM n ms
  | n == 0    = ms
  | n > 0     = dropM (n - 1) (tailM ms)
  | otherwise = error "MonadicStreams.dropM: negative argument."

-- | 'Drops' the first n elements, using a binary operation to collect
-- together the elements were dropped
dropWithM :: Monad m => (a -> a -> a) -> Int -> MonStr m a -> MonStr m a
dropWithM f n ms = (P.iterate (joinWithM f) ms) P.!! n

-- | Splits the MonStr at the given index - returns a list of monadic
-- actions
splitAtM :: Monad m => Int -> MonStr m a -> ([m a], MonStr m a)
splitAtM n ms = (takeM'' n ms, dropM n ms)

-- | Splits the MonStr at the given index - returns the elements before
-- the split inside a single monadic action
splitAtM' :: Monad m => Int -> MonStr m a -> (m [a], MonStr m a)
splitAtM' n ms = (takeM' n ms, dropM n ms)

-- | Splits the MonStr using takeF, with a Foldable type constraint
splitAtMF :: (Monad m, Foldable m) => Int -> MonStr m a -> ([m a], MonStr m a)
splitAtMF n ms = (takeMF n ms, dropM n ms)

-- | "Internal" split
splitAtML :: (Monad m, Alternative m) => Int -> MonStr m a -> (MonStr m a, MonStr m a)
splitAtML n s = (pruneL n s, dropM n s)

-- | Intersperses elements of ms with ma - works correctly for finite streams 
-- (as tested against to Data.List implementation)
intersperseMF :: (Monad m, Foldable m) => m a -> MonStr m a -> MonStr m a
intersperseMF ma = initMF'' . (intersperse ma)

-- | Interleaves monadic actions, discarding the elements in the second stream
interleaveActM :: Monad m => MonStr m a -> MonStr m b -> MonStr m a
interleaveActM (MCons ma) (MCons mb) = MCons $ do (a, ma') <- ma
                                                  (b, mb') <- mb
                                                  return (a, interleaveActM ma' mb')

-- | Interleaves monadic actions, where the actions of the second monadic stream
-- can depend on the values in the first
interleaveReadM :: Monad m => MonStr m a -> MonStr (ReaderT a m) b -> MonStr m b
interleaveReadM (MCons ma) (MCons f) = MCons $ do (a, ma') <- ma
                                                  (b, f')  <- runReaderT f a 
                                                  return (b, interleaveReadM ma' f')

-- | Inserts a given monadic action at the specified index in the stream
-- This only inserts the action, and will ignore the value of the return 
-- type
insertActM :: Monad m => Int -> m b -> MonStr m a -> MonStr m a
insertActM 0 ma mas = MCons . join $ (\(h,t) -> fmap (const (h, t)) ma) <$> uncons mas
insertActM n ma mas = MCons $ (\(h,t) -> (h, insertActM (n-1) ma t)) <$> uncons mas

-- | Inserts a 'dependent' monadic action at the specified index in the stream
-- This only inserts the action, and will ignore the value of the return 
-- type
insertActReadM :: Monad m => Int -> (a -> m b) -> MonStr m a -> MonStr m a
insertActReadM 0 f mas = MCons . join $ (\(h,t) -> fmap (const (h, t)) (f h)) <$> uncons mas
insertActReadM n f mas = MCons $ (\(h,t) -> (h, insertActReadM (n-1) f t)) <$> uncons mas

-- | Tries to break a monadic stream into its heead and tail - if the monster just consists of
-- a null element, then Nothing is returned
unconsMF :: (Monad m, Foldable m) => MonStr m a -> Maybe (m a, MonStr m a)
unconsMF ms = if (null . uncons $ ms) then Nothing else (Just (head ms, tailM ms))

-- | Returns the last element in a finite monadic stream, sequencing all
-- of the actions before
--
-- /Beware/: passing a monadic stream not containing a 'null' element 
-- will cause the function to run indefinitely
lastMF :: (Monad m, Foldable m) => MonStr m a -> m a
lastMF ms = if isEnd tl then head ms else lastMF tl
            where tl = tailM ms
                  isEnd ms' = null . uncons $ ms'

-- | Filtering the elements satisfying a predicate
-- when an element doesn't satisfy it, its children are "moved up"
-- if m is MonadPlus, there's also mfilter, but doesn't work properly on trees
--
-- /Beware/: Operating on a filtered monadic stream may diverge, if the
-- predicate was never true
filterM :: Monad m => (a -> Bool) -> MonStr m a -> MonStr m a
filterM p = mapOutM (\a s -> if p a then a <: filterM p s
                                    else filterM p s)

{-|
  Functions to separate/join words and lines within monsters of strings/characters
  Different versions may be needed for infinite monadic streams
-}

-- | Turns a stream of characters into a stream of words, delimiting with various
-- whitespace characters - these delimiters are removed
--
-- /Beware/: Doesn't pick up on thin space character '\8201', and potentially others
wordsMFL :: (Monad m, Foldable m, Alternative m) => MonStr m Char -> MonStr m String
wordsMFL mcs = if null mcs then empty
                 else mapOutM f (MCons $ fmap (\(h,t) -> (h, wordsMFL (dropM 1 t))) (spanMF (not . (`elem` [' ','\t','\n','\f','\v','\r'])) mcs))
                 where f = (\a s -> if null a then s else a <: s)

-- | Breaks up a stream of words into a stream of characters, with the words separated
-- by single space characters
unwordsMFL :: (Monad m, Foldable m, Alternative m) => MonStr m String -> MonStr m Char
unwordsMFL mss = if null mss then empty 
                   else mapOutM f (MCons $ (\(s,c) -> if null s then (' ', unwordsMFL c) else (P.head s, unwordsMFL (P.tail s <: c))) <$> uncons mss)
                   where f = (\a s -> if a == ' ' && null s then s else a <: s)

-- | Turns a stream of characters into the stream of whole lines separated by a
-- newline character - removes the newline characters
linesMFL :: (Monad m, Foldable m, Alternative m) => MonStr m Char -> MonStr m String
linesMFL mcs = if null mcs then empty
                  else MCons $ fmap (\(h,t) -> (h, linesMFL (dropM 1 t))) (spanMF (not . (== '\n')) mcs)

-- | Breaks a stream of lines up into a stream of their individual characters, separating
-- the lines with '\n' characters
unlinesMFL :: (Monad m, Foldable m, Alternative m) => MonStr m String -> MonStr m Char
unlinesMFL mss = if null mss then empty 
                    else MCons $ (\(s,c) -> if null s then ('\n', unlinesMFL c) else (P.head s, unlinesMFL (P.tail s <: c))) <$> uncons mss   

{-|
  Functions for finding indices of elements which satify given predicates
  These only work on finite monadic streams, and passing an infinite monadic stream
  will only terminate for @findIndexMMS@ and @elemIndexMMS@, unless nothing in the stream
  satisfies the given predicate
-}

-- | Finds the index of the first element satisfying the given predicate
findIndexM :: Monad m => (a -> Bool) -> MonStr m a -> m Int
findIndexM p = indexFrom 0
               where indexFrom ix mas = join $ (\(h,t) -> if p h then return ix else (indexFrom $! (ix+1)) t) <$> uncons mas

-- | Finds the index of the first element equal to the one given
elemIndexM :: (Monad m, Eq a) => a -> MonStr m a -> m Int
elemIndexM x = findIndexM (==x)

-- | Finds the indicies of the elements satisfying the given predicate
findIndicesM :: Monad m => (a -> Bool) -> MonStr m a -> MonStr m Int
findIndicesM p = indicesFrom 0
                 where indicesFrom ix mas = MCons . join $ (\(h,t) -> let ixs = (indicesFrom $! (ix+1)) t in if p h then return (ix, ixs) else (uncons ixs)) <$> uncons mas
     
-- | Finds the indicies of the elements equal to the one given
elemIndicesM :: (Monad m, Eq a) => a -> MonStr m a -> MonStr m Int
elemIndicesM x = findIndicesM (==x)
