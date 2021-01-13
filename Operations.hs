{- Operations on Monadic Streams
   Reimplementation of the functions in Data.List
   Generalization of Data.Stream (Swiestra and van Dijk)

  Notation and terminology:
  We use the same names as for the equivalent functions in Data.List
  with an -MS postfix.
  In general the result of the operation will be inside the monad;
  in cases in which it is 'pure', the postfix becomes -MMS
  Example (from MonStream.hs):
    tailMS  :: MonStr m a -> m (MonStr m a)   works for m Functor
    tailMMS :: MonStr m a -> MonStr m a       works for m Applicative
-}

module Operations where

import MonStreams
import PureStreams

import Control.Applicative
import Control.Monad

-- If m is foldable, so is (MonStr m)
--  The monster is folded in a breath-first way
instance (Functor m, Foldable m) => Foldable (MonStr m) where
  -- foldMap :: Monoid n => (a -> n) -> MonStr m a -> n
  foldMap f s = foldMap f (headMS s) `mappend`
                foldMap id (fmap (foldMap f) (tailMS s))


{- Instances of Alternative and MonadPlus: <|> and mplus are applied at top level.
   Results may be different than expected for instantiations that are
   equivalent to type with their own alternative:
   (MonStr Maybe) is equivalent to list, but <|> and `mplus` return
   the first list (if non-empty), not the concatenation.

   We mainly use empty and mzero to characterize finite monsters.
-}

instance Alternative m => Alternative (MonStr m) where
  -- empty :: MonStr m a
  empty = MCons empty

  -- (<|>) :: MonStr m a -> MonStr m a -> MonStr m a
  -- s1 <|> s2 = transformMS (\h t -> (h, t <|> s2)) s1 <|> s2
  (MCons m1) <|> (MCons m2) = MCons (m1 <|> m2)

instance MonadPlus m => MonadPlus (MonStr m) where
  -- no instantiation needed for mzero and mplus:
  --  the same as empty and <|>

{- Alternative version that graft the second stream on the zero elements
   But it also adds it at the end of the head:
   Expected result on (MonStr Maybe), but extra trees on (MonStr [])

  -- mzero :: MonStr m a
  mzero = MCons mzero

  -- mplus :: MonStr m a -> MonStr m a -> MonStr m a
  mplus s1 s2 = transformMS (\h t -> (h, mplus t s2)) s1 <|> s2
  -- mplus (MCons m1) (MCons m2) = MCons (mplus m1 m2)
-}

-- If m is Foldable, we could define alternative by grafting
graftMS :: (Functor m, Foldable m) => MonStr m a -> MonStr m a -> MonStr m a
graftMS s1 s2 = if null s1 then s2
                           else transformMS (\h t -> (h, graftMS t s2)) s1

toMonStr :: (Foldable t, Applicative m, Alternative m) => t a -> MonStr m a
toMonStr = foldr (<:) empty

-- Unfolds a monadic stream from a seed
unfoldMS :: Functor m => (b -> (m a, b)) -> b -> MonStr m a
unfoldMS f b = a <:: unfoldMS f b'
               where (a, b') = f b

-- Produces the infinite sequence of repeated applications of
--  f to x
iterateMS :: Functor m => (m a -> m a) -> m a -> MonStr m a
iterateMS f x = x <:: iterateMS f (f x)

-- Deconstructs a MonStr into its head and tail - the head
--  and tail cannot be recombined to produce the same monster
--  unless the monad satisfies certain properties
outMS :: Functor m => MonStr m a -> (m a, m (MonStr m a))
outMS ms = (headMS ms, tailMS ms)

scanMMS :: Applicative m => (a -> b -> a) -> a -> MonStr m b -> MonStr m a
scanMMS f z s = z <: scanH f z s
                where scanH f z s = MCons $ (\(a, s') -> (f z a, scanH f (f z a) s')) <$> unwrapMS s

-- A verison of scanMMS without a starting value argument - works 
--  very nicely on trees
scanMMS1 :: Monad m => (a -> a -> a) -> MonStr m a -> MonStr m a
scanMMS1 f s = absorbMS $ (\(a, s') -> scanMMS f a s') <$> unwrapMS s

-- Appending two monsters: the second is grafted when there is an empty action
infixr 5 +++
(+++) :: Alternative m => MonStr m a -> MonStr m a -> MonStr m a
s1 +++ s2 = (headMS s1 <::: ((+++) <$> tailMS s1 <*> pure s2)) <|> s2

-- indexing operator when m is a monad
infixl 9 !!!
(!!!) :: Monad m => MonStr m a -> Int -> m a
s !!! n = headMS $ (iterate tailMMS s) !! n

-- Appending an element in front of each entry in a monster of lists
consMMS :: Functor m => a -> MonStr m [a] -> MonStr m [a]
consMMS a s = fmap (a:) s

-- Accumulating the prefixes in each path of a monster:
--  Every entry contains the list of its predecessors
prefixesMMS :: Applicative m => MonStr m a -> MonStr m [a]
prefixesMMS = transformMS (\h t -> ([h], consMMS h (prefixesMMS t)))

-- Prefixes a given monadic stream with a given list of values
prefixMMS :: Applicative m => [a] -> MonStr m a -> MonStr m a
prefixMMS xs s = foldr (<:) s xs

-- Prefixes a given monadic stream with a given list of m values
prefixMS' :: Functor m => [m a] -> MonStr m a -> MonStr m a
prefixMS' xs s = foldr (<::) s xs

-- Appending the empty list, to make it equivalent to inits in Data.List
initsMMS :: Applicative m => MonStr m a -> MonStr m [a]
initsMMS s = [] <: prefixesMMS s

-- Constructs a list containing all of the suffix monsters of
--  the argument - equivalent to tails in Stream.hs
tailsMMS :: Monad m => MonStr m a -> [MonStr m a]
tailsMMS mas = mas:(tailsMMS (tailMMS mas))

-- tails with Foldable type constraint
tailsF :: (Monad m, Foldable m) => MonStr m a -> [MonStr m a]
tailsF mas = if null (unwrapMS mas) then [mas] else mas:(tailsF (tailF mas))

-- Version of "take": returns the list of the first n elements
--  For trees, we expect to get:
--    takeMMS returns the list paths of depth n
--    takeMMS'' returns the list of slices at depths less than n

-- Returns the list of first n elements
--  In case of trees: paths of length n (ignores shorter paths)
takeMMS :: Monad m => Int -> MonStr m a -> m [a]
takeMMS n s
  | n == 0 = return []
  | n > 0  = unwrapMS s >>= \(h,t) -> fmap (h:) (takeMMS (n-1) t)
  | otherwise = error "Operations.takeMMS: negative argument."

-- Simpler alternative: nth elements of the inits
takeMMS' :: Monad m => Int -> MonStr m a -> m [a]
takeMMS' n s = initsMMS s !!! n

-- Extracting the list of the first n heads (each inside the monad)
--  In case of trees: slices at depths less than n
takeMMS'' :: Monad m => Int -> MonStr m a -> [m a]
takeMMS'' n ms
  | n == 0    = []
  | n > 0     = headMS ms : (takeMMS'' (n - 1) (tailMMS ms))
  | otherwise = error "Operations.takeMMS: negative argument."

-- Take with Foldable type constraint
takeF :: (Monad m, Foldable m) => Int -> MonStr m a -> [m a]
takeF n ms
  | n == 0    = []
  | null (unwrapMS ms) = []
  | n > 0     = headMS ms : (takeF (n - 1) (tailF ms))
  | otherwise = error "Operations.takeF: negative argument."

-- Returns the longest prefix of ma that satisfies p
--  together with the remainder of the monadic stream, 
--  with the prefix inside the monad m
--
-- /Beware/: this function may diverge if every element
--  of the given monadic stream satisfies the predicate
spanMMS :: Monad m => (a -> Bool) -> MonStr m a -> m ([a], MonStr m a)
spanMMS p ma = unwrapMS ma >>= \(h,t) -> if p h then let ret = spanMMS p t in 
                                                        fmap (\(l, s) -> (h:l, s)) ret
                                                else return ([], h <: t)

-- Span with Foldable type constraint
spanF :: (Monad m, Foldable m) => (a -> Bool) -> MonStr m a -> m ([a], MonStr m a)
spanF p mas = if null ma then return ([], mas) 
                         else ma >>= \(h,t) -> if p h then let ret = spanF p t in 
                                                              fmap (\(l, s) -> (h:l, s)) ret
                                                      else return ([], h <: t)
              where ma = unwrapMS mas

-- breakMMS p is equivalent to spanMMS (not . p)
--
-- /Beware/: this function may diverge for the same reason as spanMMS
breakMMS :: Monad m => (a -> Bool) -> MonStr m a -> m ([a], MonStr m a)
breakMMS p = spanMMS (not . p)

-- This may diverge for the same reason as spanMMS
takeWhileMMS :: Monad m => (a -> Bool) -> MonStr m a -> m [a]
takeWhileMMS p ma = fmap fst $ spanMMS p ma

-- takeWhile with Foldable type constraint
takeWhileF :: (Monad m, Foldable m) => (a -> Bool) -> MonStr m a -> m [a]
takeWhileF p ma = fmap fst $ spanF p ma

-- This may diverge for the same reason as spanMMS
dropWhileMMS :: Monad m => (a -> Bool) -> MonStr m a -> MonStr m a
dropWhileMMS p ma = absorbMS . (fmap snd) $ spanMMS p ma

-- Partitions the given monadic stream into a pair of monadic streams - one where every 
--  element satisfies the predicate p, and one with the remaining elements which do not
partitionMMS :: (Monad m, Foldable m) => (a -> Bool) -> MonStr m a -> m (MonStr m a, MonStr m a)
partitionMMS p ma = unwrapMS ma >>= \(h,t) -> let ret = (if null t then pure (t, t) else partitionMMS p t) in 
                                                  if p h then (\p' -> fmap (\(t, f) -> (h <: t, f)) p') ret
                                                         else (\p' -> fmap (\(t, f) -> (t, h <: f)) p') ret

-- Groups consecutive equal elements of the monster into lists
groupMMS :: (Monad m, Eq a) => MonStr m a -> MonStr m [a]
groupMMS mas = MCons . join $ (\(h,t) -> fmap (\(h',t') -> (h:h', groupMMS t')) (spanMMS (==h) t)) <$> unwrapMS mas

-- Group with FOldable type constraint
groupF :: (Monad m, Eq a, Foldable m) => MonStr m a -> MonStr m [a]
groupF mas = MCons . join $ (\(h,t) -> fmap (\(h',t') -> (h:h', groupF t')) (spanF (==h) t)) <$> unwrapMS mas

-- isPrefixOfMMS returns True if the first argument is a prefix of the second, and False otherwise
--  will diverge if the monsters being compared are the same and are infinite
isPrefixOfMMS :: (Monad m, Foldable m, Eq a) => MonStr m a -> MonStr m a -> m Bool
isPrefixOfMMS ma mb = join $ (\(h,t) (h',t') -> if h /= h' then return False else (if null t then return True else isPrefixOfMMS t t')) <$> unwrapMS ma <*> unwrapMS mb


{- Comment by Venanzio: takeMMS' and takeMMS'' give strange results on trees
   Its a similar problem to the one we had for inits:
     extracting head and tail and then recombining means that the join
     operation generates all possible combinations
     while we want to a head to be combined only with its original tail
   Lesson: always use unwrapMS to get head and tail out
-}

-- "Internal" version of take: pruning the stream at depth n
pruneMMS :: (Functor m, Alternative m) => Int -> MonStr m a -> MonStr m a
pruneMMS n s
  | n == 0 = empty
  | n > 0  = transformMS (\h t -> (h, pruneMMS (n-1) t)) s
  | otherwise = error "Operations.pruneMMS: negative argument."

dropMMS :: Monad m => Int -> MonStr m a -> MonStr m a
dropMMS n ms
  | n == 0    = ms
  | n > 0     = dropMMS (n - 1) (tailMMS ms)
  | otherwise = error "Operations.dropMMS: negative argument."
  
-- Splits the MonStr at the given index
splitAtMMS :: Monad m => Int -> MonStr m a -> ([m a], MonStr m a)
splitAtMMS n ms = (takeMMS'' n ms, dropMMS n ms)

splitAtMMS' :: Monad m => Int -> MonStr m a -> (m [a], MonStr m a)
splitAtMMS' n ms = (takeMMS' n ms, dropMMS n ms)

-- Splits the MonStr using takeF, with a Foldable type constraint
splitAtF :: (Monad m, Foldable m) => Int -> MonStr m a -> ([m a], MonStr m a)
splitAtF n ms = (takeF n ms, dropMMS n ms)

-- "Internal" split
splitAtMS :: MonadPlus m => Int -> MonStr m a -> (MonStr m a, MonStr m a)
splitAtMS n s = (pruneMMS n s, dropMMS n s)

-- Intersperses elements of ms with ma
intersperseMS :: Functor m => m a -> MonStr m a -> MonStr m a
intersperseMS ma = transformMS (\h t -> (h, ma <:: (intersperseMS ma t)))

-- Interleaves the elements (and actions) of two monadic streams
interleaveMS :: Functor m =>  MonStr m a -> MonStr m a -> MonStr m a
interleaveMS mas mbs = transformMS (\h t -> (h, interleaveMS mbs t)) mas

-- /Beware/: passing a monadic stream not containing a 'null' element 
-- will cause the function to run indefinitely
unconsMMS :: (Monad m, Foldable m) => MonStr m a -> Maybe (m a, MonStr m a)
unconsMMS ms = if (null . unwrapMS $ ms) then Nothing else (Just (headMS ms, tailMMS ms))

-- /Beware/: passing a monadic stream not containing a 'null' element 
-- will cause the function to run indefinitely
lastMMS :: (Monad m, Foldable m) => MonStr m a -> m a
lastMMS ms = if isEnd tl then headMS ms else lastMMS tl
        where tl = tailMMS ms
              isEnd ms' = null . unwrapMS $ ms'
            
-- /Beware/: if the monster has an infinite path the function will diverge
initMMS :: (Monad m, Foldable m) => MonStr m a -> [m a]
initMMS ms = if isEnd tl then [] else (headMS ms : initMMS tl)
        where tl = tailMMS ms
              isEnd ms' = null . unwrapMS $ ms'
              
-- version of initMMS where the returned list is inside the monad  
initMMS' :: (Monad m, Foldable m) => MonStr m a -> m [a]
initMMS' = sequence . initMMS
              
-- version of initMMS where the last element is removed from the given finite MonStr
initMMS'' :: (Monad m, Foldable m) => MonStr m a -> MonStr m a
initMMS'' = mapOutMS $ \hd tl -> if null tl
                                    then tl
                                    else hd <: initMMS'' tl

-- /Beware/: passing a monadic stream not containing a 'null' element 
-- will cause the function to diverge
--  It gives the total numbers of elements in the monster
lengthMMS :: (Functor m, Foldable m) => MonStr m a -> Int
lengthMMS = length

-- Length of longest path
--  diverges if there is an infinite path
depthMMS :: (Functor m, Foldable m) => MonStr m a -> Int
depthMMS =  maximumInt . fmap (\(h,t) -> depthMMS t + 1) . unwrapMS
  where maximumInt t
          | null t = 0
          | otherwise = maximum t


-- injects an infinite repetition of a list into a monster
cycleMS :: Applicative m => [a] -> MonStr m a
cycleMS as = foldr (<:) (cycleMS as) as

-- Cycles the contents of the given list of monadic values inside a MonStr
cycleMMS :: Functor m => [m a] -> MonStr m a
cycleMMS mas = foldr (\ma s -> (ma <:: s)) (cycleMMS mas) (cycle mas)

-- A version of cycleMMS which accepts m [a] instead of [m a]
cycleMMS' :: Functor m => m [a] -> MonStr m a
cycleMMS' mas = cycleMMS (unseq (fmap cycle mas))
                where unseq ~mas' = fmap head mas' : unseq (fmap tail mas')

-- Zips the two given monsters using the argument function
--  this performs the same as liftA2 - it seems at the moment but hasn't be verified formally
zipWithMMS :: Applicative m => (a -> b -> c) -> MonStr m a -> MonStr m b -> MonStr m c
zipWithMMS = liftA2

zipWith3MMS :: Applicative m => (a -> b -> c -> d) -> MonStr m a -> MonStr m b -> MonStr m c -> MonStr m d
zipWith3MMS f ma mb mc = f <$> ma <*> mb <*> mc

-- Takes two monadic streams and returns a monster of pairs obtained
-- by pairing elements at the same index in the argument monsters
zipMMS :: Applicative m => MonStr m a -> MonStr m b -> MonStr m (a,b)
zipMMS = zipWithMMS (,)

zip3MMS :: Applicative m => MonStr m a -> MonStr m b -> MonStr m c -> MonStr m (a,b,c)
zip3MMS = zipWith3MMS (,,)

-- Inverse operations of zipMMS and zip3MMS
unzipMMS :: Applicative m => MonStr m (a, b) -> (MonStr m a, MonStr m b)
unzipMMS ms = (fmap fst ms, fmap snd ms)

unzip3MMS :: Applicative m => MonStr m (a, b, c) -> (MonStr m a, MonStr m b,  MonStr m c)
unzip3MMS ms = (fmap (\(a,_,_) -> a) ms, fmap (\(_,b,_) -> b) ms, fmap (\(_,_,c) -> c) ms)

-- Filtering the elements satisfying a predicate
--  when an element doesn't satisfy it, its children are "moved up"
--  if m is MonadPlus, there's also mfilter, but doesn't work properly on trees
filterMMS :: Monad m => (a -> Bool) -> MonStr m a -> MonStr m a
filterMMS p = mapOutMS (\a s -> if p a then a <: filterMMS p s
                                       else filterMMS p s)

-- Functions to separate/join words and lines within monsters of strings/characters
                                       
wordsMMS :: Monad m => MonStr m Char -> MonStr m String
wordsMMS mcs = MCons $ do (c,s) <- unwrapMS mcs
                          if c == ' ' then return ([], wordsMMS s)
                                      else fmap (\(str, s') -> (c:str, s')) (unwrapMS (wordsMMS s))
                     
unwordsMMS :: Monad m => MonStr m String -> MonStr m Char
unwordsMMS mss = MCons $ (\(s,c) -> if null s then (' ', unwordsMMS c) else (head s, unwordsMMS (tail s <: c))) <$> unwrapMS mss    

linesMMS :: Monad m => MonStr m Char -> MonStr m String
linesMMS mcs = MCons $ do (c,s) <- unwrapMS mcs
                          if c == '\n' then return ([], linesMMS s)
                                      else fmap (\(str, s') -> (c:str, s')) (unwrapMS (linesMMS s))
                     
unlinesMMS :: Monad m => MonStr m String -> MonStr m Char
unlinesMMS mss = MCons $ (\(s,c) -> if null s then ('\n', unlinesMMS c) else (head s, unlinesMMS (tail s <: c))) <$> unwrapMS mss                              

-- Functions for finding indices of elements which satify given predicates

findIndexMMS :: Monad m => (a -> Bool) -> MonStr m a -> m Int
findIndexMMS p = indexFrom 0
                 where indexFrom ix mas = join $ (\(h,t) -> if p h then return ix else (indexFrom $! (ix+1)) t) <$> unwrapMS mas

elemIndexMMS :: (Monad m, Eq a) => a -> MonStr m a -> m Int
elemIndexMMS x = findIndexMMS (==x)

findIndicesMMS :: Monad m => (a -> Bool) -> MonStr m a -> MonStr m Int
findIndicesMMS p = indicesFrom 0
                   where indicesFrom ix mas = MCons . join $ (\(h,t) -> let ixs = (indicesFrom $! (ix+1)) t in if p h then return (ix, ixs) else (unwrapMS ixs)) <$> unwrapMS mas
                  
elemIndicesMMS :: (Monad m, Eq a) => a -> MonStr m a -> MonStr m Int
elemIndicesMMS x = findIndicesMMS (==x)