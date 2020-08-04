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
  -- s1 <|> s2 = transformMS (\h t -> h) (\h t -> t <|> s2) s1 <|> s2
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
  mplus s1 s2 = transformMS (\h t -> h) (\h t -> mplus t s2) s1 <|> s2
  -- mplus (MCons m1) (MCons m2) = MCons (mplus m1 m2)
-}

-- If m is Foldable, we could define alternative by grafting
graftMS :: (Functor m, Foldable m) => MonStr m a -> MonStr m a -> MonStr m a
graftMS s1 s2 = if null s1 then s2
                           else transformMS (\h t -> h)
                                            (\h t -> graftMS t s2)
                                            s1


-- Appending two monsters: the second is grafted when there is an empty action
infixr 5 +++
(+++) :: Alternative m => MonStr m a -> MonStr m a -> MonStr m a
s1 +++ s2 = (headMS s1 <::: ((+++) <$> tailMS s1 <*> pure s2)) <|> s2


-- indexing operator when m is a monad
infixl 9 !!!
(!!!) :: Monad m => MonStr m a -> Int -> m a
s !!! n = headMS $ (iterate tailMMS s) !! n

-- Appending an element in from of each entry in a monster of lists
consMMS :: Functor m => a -> MonStr m [a] -> MonStr m [a]
consMMS a s = fmap (a:) s

-- Accumulating the prefixes in each path of a monster:
--  Every entry contains the list of its predecessors
prefixesMMS :: Applicative m => MonStr m a -> MonStr m [a]
prefixesMMS = transformMS (\h t -> [h]) (\h t -> consMMS h (prefixesMMS t))

-- Appending the empty list, to make it equivalente to inits in Data.List
initsMMS :: Applicative m => MonStr m a -> MonStr m [a]
initsMMS s = [] <: prefixesMMS s


-- Version of "take": returns the list of the first n elements
--  For trees, we expect to get:
--    takeMS returns the list paths of depth n
--    takeMMS returns the list of slices at depths less than n

-- Returns the list of first n elements
--  In case of trees: paths of length n (ignores shorter paths)
takeMS :: Monad m => Int -> MonStr m a -> m [a]
takeMS n s
  | n == 0 = return []
  | n > 0  = unwrapMS s >>= \(h,t) -> fmap (h:) (takeMS (n-1) t)
  | otherwise = error "Operations.takeMS: negative argument."

-- Simpler alternative: nth elements of the inits
takeMS' :: Monad m => Int -> MonStr m a -> m [a]
takeMS' n s = initsMMS s !!! n

-- Extracting the list of the first n heads (each inside the monad)
--  In case of trees: slices at depths less than n
takeMMS :: Monad m => Int -> MonStr m a -> [m a]
takeMMS n ms
  | n == 0    = []
  | n > 0     = headMS ms : (takeMMS (n - 1) (tailMMS ms))
  | otherwise = error "Operations.takeMMS: negative argument."


{- Comment by Venanzio: takeMMS' and takeMMS'' give strange results on trees
   Its a similar problem to the one we had for inits:
     extracting head and tail and then recombining means that the join
     operation generates all possible combinations
     while we want to a head to be combined only with its original tail
   Lesson: always use unwrapMS to get head and tail out
-}

-- version of takeMMS where the returned list is inside the monad, rather 
-- than a list of monadic actions
takeMMS' :: Monad m => Int -> MonStr m a -> m [a]
takeMMS' n ms
  | n == 0    = return []
  | n > 0     = do a <- headMS ms
                   (a:) <$> takeMMS' (n-1) (tailMMS ms)
  | otherwise = error "Operations.takeMMS': negative argument."
  
-- version of the above using sequence instead to make more compact
takeMMS'' :: Monad m => Int -> MonStr m a -> m [a]
takeMMS'' n = sequence . (takeMMS n)

-- "Internal" version of take: pruning the stream at depth n
pruneMMS :: (Functor m, Alternative m) => Int -> MonStr m a -> MonStr m a
pruneMMS n s
  | n == 0 = empty
  | n > 0  = transformMS (\h _ -> h) (\_ t -> pruneMMS (n-1) t) s
  | otherwise = error "Operations.pruneMMS: negative argument."


dropMMS :: Monad m => Int -> MonStr m a -> MonStr m a
dropMMS n ms
  | n == 0    = ms
  | n > 0     = dropMMS (n - 1) (tailMMS ms)
  | otherwise = error "Operations.dropMMS: negative argument."
  
-- Splits the MonStr at the given index
splitAtMMS :: Monad m => Int -> MonStr m a -> ([m a], MonStr m a)
splitAtMMS n ms = (takeMMS n ms, dropMMS n ms)

splitAtMMS' :: Monad m => Int -> MonStr m a -> (m [a], MonStr m a)
splitAtMMS' n ms = (takeMS' n ms, dropMMS n ms)




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
            
-- /Beware/: passing a monadic stream not containing a 'null' element 
-- will cause the function to run indefinitely  
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
-- will cause the function to run indefinitely
lengthMMS :: (Monad m, Foldable m) => MonStr m a -> Int
lengthMMS = length

-- Cycles the contents of the given list of monadic values inside a MonStr
cycleMMS :: Functor m => [m a] -> MonStr m a
cycleMMS mas = foldr (\ma s -> (ma <:: s)) (cycleMMS mas) (cycle mas)

-- A version of cycleMMS which accepts m [a] instead of [m a]
cycleMMS' :: Functor m => m [a] -> MonStr m a
cycleMMS' mas = cycleMMS (unseq (fmap cycle mas))
                where unseq ~mas' = fmap head mas' : unseq (fmap tail mas')

-- Zips the two given monsters using the argument function
zipWithMMS :: Applicative m => (a -> b -> c) -> MonStr m a -> MonStr m b -> MonStr m c
zipWithMMS f (MCons ma) (MCons mb) = MCons $ liftA2 (\a b -> (f (fst a) (fst b), zipWithMMS f (snd a) (snd b))) ma mb

zipWith3MMS :: Applicative m => (a -> b -> c -> d) -> MonStr m a -> MonStr m b -> MonStr m c -> MonStr m d
zipWith3MMS f (MCons ma) (MCons mb) (MCons mc) = MCons $ (\a b c -> (f (fst a) (fst b) (fst c), zipWith3MMS f (snd a) (snd b) (snd c))) <$> ma <*> mb <*> mc

-- Takes two monadic streams and returns a monster of pairs obtained
-- by pairing elements at the same index in the argument monsters
zipMMS :: Applicative m => MonStr m a -> MonStr m b -> MonStr m (a,b)
zipMMS = zipWithMMS (,)

zip3MMS :: Applicative m => MonStr m a -> MonStr m b -> MonStr m c -> MonStr m (a,b,c)
zip3MMS = zipWith3MMS (,,)

-- Currently doesn't seem possible, may benefit from looking at Data.Predicate
-- filterMMS :: Monad m => (a -> Bool) -> MonStr m a -> MonStr m a
-- filterMMS p ms = do a <- headMS ms
--                     if (p a) then (MCons $ return (a, filterMMS p tl))
--                              else filterMMS p tl
--                  where tl = tailMMS ms
