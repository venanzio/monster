module MonadicStream
   (
   -- The type of monadic streams
     MonStr(..),
     
     unwrap,
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
     
     repeatA,
     liftM,
     absorbM,
     graftF,
     toMonStr,
     unfold,
     iterate,
     decomp,
     mapOutM,
     scanA,
     scanM,
     (!!),
     cons,
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
     pruneL,
     dropM,
     
     splitAtM,
     splitAtM',
     splitAtMF,
     splitAtML,
     
     intersperse,
     intersperseMF,
     interleave,
     
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
import qualified Prelude as P ((!!), iterate, head, tail, cycle)
 
import Control.Applicative
import Control.Monad hiding (liftM, filterM)
import Control.Comonad

-- | Type of monadic streams, m is not required 
-- to be a monad - only a functor. 
-- Terminology note: "m-monster" or "m-stream" 
-- refers to a monadic stream under a functor
-- m; an element of MonStr m a
data MonStr m a = MCons (m (a , MonStr m a))

{-
  Function naming conventions
  ---------------------------
   A function name may have one or more letters suffixed, indicating 
   what kinds of underlying functors the function is designed to 
   operate on:
    - A = Applicative
    - M = Monad
    - F = Foldable
    - L = Alternative
   
   A lack of any letters indicates that the function works on regular
   functors, and so can be used with any monadic stream.

   The functions !! and ++ do not follow this pattern for style reasons:
    - !! works for m Monad
    - ++ works for m Alternative
-}

{-
 Basic deconstructors and convenience functions for
 appending elements to monsters etc.
-}

unwrap :: MonStr m a -> m (a, MonStr m a)
unwrap (MCons m) = m  

head :: Functor m => MonStr m a -> m a
head = fmap fst . unwrap

tail :: Functor m => MonStr m a -> m (MonStr m a)
tail = fmap snd . unwrap

-- | Tail for m Monad - joins the monadic action, returning a tail
-- that isn't wrapped in the functor
tailM :: Monad m => MonStr m a -> MonStr m a
tailM = MCons . join . fmap unwrap . tail

-- | Tail for m Monad and Foldable - deals with empty tails
tailMF :: (Monad m, Foldable m) => MonStr m a -> MonStr m a
tailMF ma = let t = tail ma in
              if null t then error "Empty monadic stream" else (MCons . join . fmap unwrap) t
              
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


-- Class instance declarations
----------------------------------

-- | Transform a monster by mapping head and tail to new 
-- head and tail under f
transform :: Functor m => 
               (a -> MonStr m a -> (b, MonStr m b)) ->
               MonStr m a -> MonStr m b
transform f s = MCons $ fmap (\(h,t) -> f h t) (unwrap s)

instance Functor m => Functor (MonStr m) where
  fmap f = transform (\a s -> (f a, fmap f s))


-- | Version of transform for applicative monsters
transformA :: Applicative m =>
               (a -> MonStr m a -> b -> MonStr m b -> (c, MonStr m c)) ->
               MonStr m a -> MonStr m b -> MonStr m c
transformA f as bs = MCons $ (\(a,as') (b,bs') -> f a as' b bs')
                               <$> unwrap as <*> unwrap bs

instance Applicative m => Applicative (MonStr m) where
  pure a = a <: pure a
  (<*>) = transformA (\f fs a as -> (f a, fs <*> as))  


instance Comonad w => Comonad (MonStr w) where
  extract = extract . head
  duplicate (MCons s) = MCons $ fmap (\(h,t) -> (MCons s, duplicate t)) s


instance (Functor m, Foldable m) => Foldable (MonStr m) where
  foldMap f s = foldMap f (head s) `mappend`
                foldMap id (fmap (foldMap f) (tail s))


instance Alternative m => Alternative (MonStr m) where
  empty = MCons empty
  (MCons m1) <|> (MCons m2) = MCons (m1 <|> m2)


-- Operations
-----------------

-- | Repeats an element and turns it into a monadic stream - requires
-- m Applicative
repeatA :: Applicative m => a -> MonStr m a
repeatA = pure

-- | Lifts monadic actions from elements to the stream
liftM :: Monad m => MonStr m (m a) -> m (MonStr m a)
liftM ms = pure (\(ma,ms') -> ma <::: liftM ms') <*> unwrap ms

-- | Monadic monsters can "absorb" a monadic action
absorbM :: Monad m => m (MonStr m a) -> MonStr m a
absorbM ms = MCons . join $ fmap unwrap ms

-- | If m is Foldable, we could define alternative by grafting
graftF :: (Functor m, Foldable m) => MonStr m a -> MonStr m a -> MonStr m a
graftF s1 s2 = if null s1 then s2
                          else transform (\h t -> (h, graftF t s2)) s1

-- | Converts a foldable container to a monadic stream
toMonStr :: (Foldable t, Applicative m, Alternative m) => t a -> MonStr m a
toMonStr = foldr (<:) empty

-- | Unfolds a monadic stream from a seed value - monadic stream co-iterator
unfold :: Functor m => (b -> (m a, b)) -> b -> MonStr m a
unfold f b = a <:: unfold f b'
             where (a, b') = f b

-- | Produces an infinite sequence of repeated applications of f to x
iterate :: Functor m => (m a -> m a) -> m a -> MonStr m a
iterate f x = x <:: iterate f (f x)

-- | Deconstructs a MonStr into its head and tail - the head
-- and tail cannot be recombined to produce the same monster
-- unless the monad satisfies certain properties
decomp :: Functor m => MonStr m a -> (m a, m (MonStr m a))
decomp ms = (head ms, tail ms)

-- | Transform a monadic monster by operating on the "raw" head 
-- and tail
mapOutM :: Monad m => (a -> MonStr m a -> MonStr m b) ->
                       MonStr m a -> MonStr m b
mapOutM f s = absorbM $ fmap (uncurry f) (unwrap s)

--
scanA :: Applicative m => (a -> b -> a) -> a -> MonStr m b -> MonStr m a
scanA f z s = z <: scanH f z s
              where scanH f z s = MCons $ (\(a, s') -> (f z a, scanH f (f z a) s')) <$> unwrap s

-- | A verison of scan without a starting value argument - works 
-- very nicely on List-monsters (trees)
scanM :: Monad m => (a -> a -> a) -> MonStr m a -> MonStr m a
scanM f s = absorbM $ (\(a, s') -> scanA f a s') <$> unwrap s

-- | Appending two monsters: the second is grafted when there is an empty action,
-- require
infixr 5 ++
(++) :: Alternative m => MonStr m a -> MonStr m a -> MonStr m a
s1 ++ s2 = (head s1 <::: ((++) <$> tail s1 <*> pure s2)) <|> s2

-- | Indexing operator with m Monad
infixl 9 !!
(!!) :: Monad m => MonStr m a -> Int -> m a
s !! n = head $ (P.iterate tailM s) P.!! n

-- | Appending an element in front of each element in a monster containing lists
cons :: Functor m => a -> MonStr m [a] -> MonStr m [a]
cons a s = fmap (a:) s

-- | Prefixes a given monadic stream with a given list of values
prefixA :: Applicative m => [a] -> MonStr m a -> MonStr m a
prefixA xs s = foldr (<:) s xs

-- | Prefixes a given monadic stream with a given list of m values
prefix :: Functor m => [m a] -> MonStr m a -> MonStr m a
prefix xs s = foldr (<::) s xs

-- |
--
-- /Beware/: if the monster has an infinite path the function will diverge
initMF :: (Monad m, Foldable m) => MonStr m a -> [m a]
initMF ms = if isEnd tl then [] else (head ms : initMF tl)
            where tl = tailM ms
                  isEnd ms' = null . unwrap $ ms'
              
-- | Version of init where the returned list is inside a single monadic action
initMF' :: (Monad m, Foldable m) => MonStr m a -> m [a]
initMF' = sequence . initMF
              
-- version of init where the last element is removed from the given finite MonStr
initMF'' :: (Monad m, Foldable m) => MonStr m a -> MonStr m a
initMF'' = mapOutM $ \hd tl -> if null tl
                                  then tl
                                  else hd <: initMF'' tl

-- | Accumulating the prefixes in each path of a monster:
-- Every entry contains the list of its predecessors - the 
-- empty list is appended, to make it equivalent to inits 
-- in Data.List
initsA :: Applicative m => MonStr m a -> MonStr m [a]
initsA s = [] <: initsA' s
           where initsA' = transform (\h t -> ([h], cons h (initsA' t)))

-- | Constructs a list containing all of the suffix monsters of
-- the argument - equivalent to tails in Stream.hs
tailsM :: Monad m => MonStr m a -> [MonStr m a]
tailsM mas = mas : (tailsM (tailM mas))

-- | tails with Foldable type constraint
tailsMF :: (Monad m, Foldable m) => MonStr m a -> [MonStr m a]
tailsMF mas = if null (unwrap mas) then [mas] else mas : (tailsMF (tailMF mas))

-- Version of "take": returns the list of the first n elements
--  For trees, we expect to get:
--    takeMMS returns the list paths of depth n
--    takeMMS'' returns the list of slices at depths less than n

-- | Returns the list of first n elements. In the case of 
-- List-monsters (trees): paths of length n (ignores 
-- shorter paths)
takeM :: Monad m => Int -> MonStr m a -> m [a]
takeM n s
  | n == 0 = return []
  | n > 0  = unwrap s >>= \(h,t) -> fmap (h:) (takeM (n-1) t)
  | otherwise = error "MonadicStream.takeM: negative argument."

-- | Simpler alternative to takeM: nth elements of the inits
takeM' :: Monad m => Int -> MonStr m a -> m [a]
takeM' n s = initsA s !! n

-- | Extracting the list of the first n heads (each inside the monad)
-- In case of trees: slices at depths less than n
takeM'' :: Monad m => Int -> MonStr m a -> [m a]
takeM'' n ms
  | n == 0    = []
  | n > 0     = head ms : (takeM'' (n - 1) (tailM ms))
  | otherwise = error "MonadicStream.takeM'': negative argument."

-- | take with  m Foldable - handles finite streams
takeMF :: (Monad m, Foldable m) => Int -> MonStr m a -> [m a]
takeMF n ms
  | n == 0    = []
  | null (unwrap ms) = []
  | n > 0     = head ms : (takeMF (n - 1) (tailMF ms))
  | otherwise = error "MonadicStream.takeMF: negative argument."

-- | Returns the longest prefix of ma that satisfies p
-- together with the remainder of the monadic stream, 
-- with the prefix inside the monad m
--
-- /Beware/: this function may diverge if every element
--  of the given monadic stream satisfies the predicate
spanM :: Monad m => (a -> Bool) -> MonStr m a -> m ([a], MonStr m a)
spanM p ma = unwrap ma >>= \(h,t) -> if p h then let ret = spanM p t in 
                                                     fmap (\(l, s) -> (h:l, s)) ret
                                            else return ([], h <: t)

-- | span with Foldable type constraint
spanMF :: (Monad m, Foldable m) => (a -> Bool) -> MonStr m a -> m ([a], MonStr m a)
spanMF p mas = if null ma then return ([], mas) 
                          else ma >>= \(h,t) -> if p h then let ret = spanMF p t in 
                                                                fmap (\(l, s) -> (h:l, s)) ret
                                                       else return ([], h <: t)
               where ma = unwrap mas

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
partitionMF p ma = unwrap ma >>= \(h,t) -> let ret = (if null t then pure (t, t) else partitionMF p t) in 
                                               if p h then (\p' -> fmap (\(t, f) -> (h <: t, f)) p') ret
                                                      else (\p' -> fmap (\(t, f) -> (t, h <: f)) p') ret

-- | Groups consecutive equal elements of a monadic monster into lists
groupM :: (Monad m, Eq a) => MonStr m a -> MonStr m [a]
groupM mas = MCons . join $ (\(h,t) -> fmap (\(h',t') -> (h:h', groupM t')) (spanM (==h) t)) <$> unwrap mas

-- | Group with m Foldable
groupMF :: (Monad m, Eq a, Foldable m) => MonStr m a -> MonStr m [a]
groupMF mas = MCons . join $ (\(h,t) -> fmap (\(h',t') -> (h:h', groupMF t')) (spanMF (==h) t)) <$> unwrap mas

-- | isPrefixOfMMS returns True if the first argument is a prefix of the second, and False otherwise
-- will diverge if the monsters being compared are the same and are infinite
isPrefixOfMF :: (Monad m, Foldable m, Eq a) => MonStr m a -> MonStr m a -> m Bool
isPrefixOfMF ma mb = if null ma 
                        then return True 
                        else join $ (\(h,t) (h',t') -> if h /= h' then return False else (if null t then return True else isPrefixOfMF t t')) <$> unwrap ma <*> unwrap mb

-- | "Internal" version of take: pruning the stream at depth n
pruneL :: (Functor m, Alternative m) => Int -> MonStr m a -> MonStr m a
pruneL n s
  | n == 0 = empty
  | n > 0  = transform (\h t -> (h, pruneL (n-1) t)) s
  | otherwise = error "MonadicStream.pruneFL: negative argument."

--
dropM :: Monad m => Int -> MonStr m a -> MonStr m a
dropM n ms
  | n == 0    = ms
  | n > 0     = dropM (n - 1) (tailM ms)
  | otherwise = error "MonadicStream.dropM: negative argument."
  
-- | Splits the MonStr at the given index
splitAtM :: Monad m => Int -> MonStr m a -> ([m a], MonStr m a)
splitAtM n ms = (takeM'' n ms, dropM n ms)

-- |
splitAtM' :: Monad m => Int -> MonStr m a -> (m [a], MonStr m a)
splitAtM' n ms = (takeM' n ms, dropM n ms)

-- | Splits the MonStr using takeF, with a Foldable type constraint
splitAtMF :: (Monad m, Foldable m) => Int -> MonStr m a -> ([m a], MonStr m a)
splitAtMF n ms = (takeMF n ms, dropM n ms)

-- | "Internal" split
splitAtML :: (Monad m, Alternative m) => Int -> MonStr m a -> (MonStr m a, MonStr m a)
splitAtML n s = (pruneL n s, dropM n s)

-- | Intersperses elements of ms with ma - works better for infinite streams
intersperse :: Functor m => m a -> MonStr m a -> MonStr m a
intersperse ma = transform (\h t -> (h, ma <:: (intersperse ma t)))

-- | Intersperses elements of ms with ma - works correctly for finite streams 
-- (as tested against to Data.List implementation)
intersperseMF :: (Monad m, Foldable m) => m a -> MonStr m a -> MonStr m a
intersperseMF ma = initMF'' . (intersperse ma)

-- | Interleaves the elements (and actions) of two monadic streams
interleave :: Functor m =>  MonStr m a -> MonStr m a -> MonStr m a
interleave mas mbs = transform (\h t -> (h, interleave mbs t)) mas

-- |
--
-- /Beware/: passing a monadic stream not containing a 'null' element 
-- will cause the function to run indefinitely
unconsMF :: (Monad m, Foldable m) => MonStr m a -> Maybe (m a, MonStr m a)
unconsMF ms = if (null . unwrap $ ms) then Nothing else (Just (head ms, tailM ms))

-- |
--
-- /Beware/: passing a monadic stream not containing a 'null' element 
-- will cause the function to run indefinitely
lastMF :: (Monad m, Foldable m) => MonStr m a -> m a
lastMF ms = if isEnd tl then head ms else lastMF tl
            where tl = tailM ms
                  isEnd ms' = null . unwrap $ ms'
                  
-- | Gives the total numbers of elements in a monster
--
-- /Beware/: passing a monadic stream not containing a 'null' element 
-- will cause the function to diverge
lengthF :: (Functor m, Foldable m) => MonStr m a -> Int
lengthF = length

-- | Length of longest path - diverges if there is an infinite path
depthF :: (Functor m, Foldable m) => MonStr m a -> Int
depthF = maximumInt . fmap (\(h,t) -> depthF t + 1) . unwrap
         where maximumInt t | null t = 0
                            | otherwise = maximum t

-- | Injects an infinite repetition of a list into a monster
cycleA :: Applicative m => [a] -> MonStr m a
cycleA as = foldr (<:) (cycleA as) as

-- | Cycles the contents of the given list of monadic values inside a MonStr
cycle :: Functor m => [m a] -> MonStr m a
cycle mas = foldr (\ma s -> (ma <:: s)) (cycle mas) (P.cycle mas)

-- | A version of cycleMMS which accepts m [a] instead of [m a]
cycle' :: Functor m => m [a] -> MonStr m a
cycle' mas = cycle (unseq (fmap P.cycle mas))
             where unseq ~mas' = fmap P.head mas' : unseq (fmap P.tail mas')
             
-- | Filtering the elements satisfying a predicate
-- when an element doesn't satisfy it, its children are "moved up"
-- if m is MonadPlus, there's also mfilter, but doesn't work properly on trees
filterM :: Monad m => (a -> Bool) -> MonStr m a -> MonStr m a
filterM p = mapOutM (\a s -> if p a then a <: filterM p s
                                    else filterM p s)

-- | Zips the two given monsters using the argument function
-- this performs the same as liftA2 - it seems at the moment but hasn't be verified formally
zipWithA :: Applicative m => (a -> b -> c) -> MonStr m a -> MonStr m b -> MonStr m c
zipWithA = liftA2

-- |
zipWith3A :: Applicative m => (a -> b -> c -> d) -> MonStr m a -> MonStr m b -> MonStr m c -> MonStr m d
zipWith3A f ma mb mc = f <$> ma <*> mb <*> mc

-- | Takes two monadic streams and returns a monster of pairs obtained
-- by pairing elements at the same index in the argument monsters
zipA :: Applicative m => MonStr m a -> MonStr m b -> MonStr m (a,b)
zipA = zipWithA (,)

-- |
zip3A :: Applicative m => MonStr m a -> MonStr m b -> MonStr m c -> MonStr m (a,b,c)
zip3A = zipWith3A (,,)

-- | Inverse operations of zipMMS and zip3MMS
unzipA :: Applicative m => MonStr m (a, b) -> (MonStr m a, MonStr m b)
unzipA ms = (fmap fst ms, fmap snd ms)

-- |
unzip3A :: Applicative m => MonStr m (a, b, c) -> (MonStr m a, MonStr m b,  MonStr m c)
unzip3A ms = (fmap (\(a,_,_) -> a) ms, fmap (\(_,b,_) -> b) ms, fmap (\(_,_,c) -> c) ms)

{-
  Functions to separate/join words and lines within monsters of strings/characters
  Different versions may be needed for infinite monadic streams
-}

-- | 
--
-- /Beware/: Doesn't pick up on thin space character '\8201'
wordsMFL :: (Monad m, Foldable m, Alternative m) => MonStr m Char -> MonStr m String
wordsMFL mcs = if null mcs then empty
                 else mapOutM f (MCons $ fmap (\(h,t) -> (h, wordsMFL (dropM 1 t))) (spanMF (not . (`elem` [' ','\t','\n','\f','\v','\r'])) mcs))
                 where f = (\a s -> if null a then s else a <: s)

-- |
unwordsMFL :: (Monad m, Foldable m, Alternative m) => MonStr m String -> MonStr m Char
unwordsMFL mss = if null mss then empty 
                   else mapOutM f (MCons $ (\(s,c) -> if null s then (' ', unwordsMFL c) else (P.head s, unwordsMFL (P.tail s <: c))) <$> unwrap mss)
                   where f = (\a s -> if a == ' ' && null s then s else a <: s)

-- |
linesMFL :: (Monad m, Foldable m, Alternative m) => MonStr m Char -> MonStr m String
linesMFL mcs = if null mcs then empty
                  else MCons $ fmap (\(h,t) -> (h, linesMFL (dropM 1 t))) (spanMF (not . (== '\n')) mcs)

-- |              
unlinesMFL :: (Monad m, Foldable m, Alternative m) => MonStr m String -> MonStr m Char
unlinesMFL mss = if null mss then empty 
                    else MCons $ (\(s,c) -> if null s then ('\n', unlinesMFL c) else (P.head s, unlinesMFL (P.tail s <: c))) <$> unwrap mss   

{-
  Functions for finding indices of elements which satify given predicates
  These only work on finite monadic streams, and passing an infinite monadic stream
  will only terminate for findIndexMMS and elemIndexMMS, in the case that something
  satisfying the predicate is found
-}

-- |
findIndexM :: Monad m => (a -> Bool) -> MonStr m a -> m Int
findIndexM p = indexFrom 0
               where indexFrom ix mas = join $ (\(h,t) -> if p h then return ix else (indexFrom $! (ix+1)) t) <$> unwrap mas

-- |
elemIndexM :: (Monad m, Eq a) => a -> MonStr m a -> m Int
elemIndexM x = findIndexM (==x)

-- |
findIndicesM :: Monad m => (a -> Bool) -> MonStr m a -> MonStr m Int
findIndicesM p = indicesFrom 0
                 where indicesFrom ix mas = MCons . join $ (\(h,t) -> let ixs = (indicesFrom $! (ix+1)) t in if p h then return (ix, ixs) else (unwrap ixs)) <$> unwrap mas
     
-- |             
elemIndicesM :: (Monad m, Eq a) => a -> MonStr m a -> MonStr m Int
elemIndicesM x = findIndicesM (==x)
