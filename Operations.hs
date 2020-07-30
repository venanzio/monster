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



initsMS :: Monad m => MonStr m a -> MonStr m [a]
initsMS s = (fmap (\a -> [a]) (headMS s)) <:::
            (consMS <$> (headMS s) <*> (fmap initsMS (tailMS s)))

consMS :: Functor m => a -> MonStr m [a] -> MonStr m [a]
consMS a s = fmap (a:) s

inits :: Monad m => MonStr m a -> MonStr m [a]
inits s = [] <: initsMS s

takeMS :: Monad m => Int -> MonStr m a  -> [m a]
takeMS n ~ms
  | n == 0    = []
  | n > 0     =  headMS ms : (takeMS (n - 1) (tailMMS ms))
  | otherwise = error "Operations.takeMS: negative argument."
  
dropMS :: Monad m => Int -> MonStr m a -> MonStr m a
dropMS n ~ms
  | n == 0    = ms
  | n > 0     = dropMS (n - 1) (tailMMS ms)
  | otherwise = error "Operations.dropMS: negative argument."