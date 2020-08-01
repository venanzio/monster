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
  
dropMMS :: Monad m => Int -> MonStr m a -> MonStr m a
dropMMS n ms
  | n == 0    = ms
  | n > 0     = dropMMS (n - 1) (tailMMS ms)
  | otherwise = error "Operations.dropMMS: negative argument."

-- indexing operator when m is a monad
infixl 9 !!!
(!!!) :: Monad m => MonStr m a -> Int -> m a
s !!! n = headMS $ (iterate tailMMS s) !! n

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
initMMS' ms = if isEnd tl 
                 then 
                    return [] 
                 else 
                    do a <- headMS ms
                       (a:) <$> initMMS' tl
        where tl = tailMMS ms
              isEnd ms' = null . unwrapMS $ ms'

-- cycle :: [a] -> Stream a
-- cycle xs = foldr Cons (cycle xs) xs
