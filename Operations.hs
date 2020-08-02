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

instance Alternative m => Alternative (MonStr m) where
  -- empty :: MonStr m a
  empty = MCons empty

  -- (<|>) :: MonStr m a -> MonStr m a -> MonStr m a
  (MCons m1) <|> (MCons m2) = MCons (m1 <|> m2)
  

-- Appending two monsters: the second is grafted when there is an empty action
infixr 5 +++
(+++) :: Alternative m => MonStr m a -> MonStr m a -> MonStr m a
s1 +++ s2 = (headMS s1 <::: ((+++) <$> tailMS s1 <*> pure s2)) <|> s2

-- If m is foldable, so is (MonStr m)
instance (Functor m, Foldable m) => Foldable (MonStr m) where
  -- foldMap :: Monoid n => (a -> n) -> MonStr m a -> n
  foldMap f s = foldMap f (headMS s) `mappend`
                foldMap id (fmap (foldMap f) (tailMS s))

consMMS :: Functor m => a -> MonStr m [a] -> MonStr m [a]
consMMS a s = fmap (a:) s

prefixesMMS :: Applicative m => MonStr m a -> MonStr m [a]
prefixesMMS s = MCons $ inms <$> (unwrapMS s)
  where inms (h,t) = ([h], consMMS h (prefixesMMS t))
  
initsMMS :: Applicative m => MonStr m a -> MonStr m [a]
initsMMS s = [] <: prefixesMMS s

takeMMS :: Monad m => Int -> MonStr m a -> [m a]
takeMMS n ms
  | n == 0    = []
  | n > 0     = headMS ms : (takeMMS (n - 1) (tailMMS ms))
  | otherwise = error "Operations.takeMMS: negative argument."
  
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
  
dropMMS :: Monad m => Int -> MonStr m a -> MonStr m a
dropMMS n ms
  | n == 0    = ms
  | n > 0     = dropMMS (n - 1) (tailMMS ms)
  | otherwise = error "Operations.dropMMS: negative argument."
  
-- Splits the MonStr at the given index
splitAtMMS :: Monad m => Int -> MonStr m a -> ([m a], MonStr m a)
splitAtMMS n ms = (takeMMS n ms, dropMMS n ms)

splitAtMMS' :: Monad m => Int -> MonStr m a -> (m [a], MonStr m a)
splitAtMMS' n ms = (takeMMS' n ms, dropMMS n ms)

-- indexing operator when m is a monad
infixl 9 !!!
(!!!) :: Monad m => MonStr m a -> Int -> m a
s !!! n = headMS $ (iterate tailMMS s) !! n

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
initMMS'' ms = if isEnd tl then tl else MCons (fmap (\(a, s) -> (a, initMMS'' s)) $ unwrapMS ms)
        where tl = tailMMS ms
              isEnd ms' = null . unwrapMS $ ms'

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
