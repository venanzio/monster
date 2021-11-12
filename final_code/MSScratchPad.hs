import MonadicStreams hiding ((<<*>>))
import Examples.LazyLists
import Data.Bifunctor
import Data.Bifunctor.Apply ((<<$>>))
-- import FurtherTypeClasses

-- A recursive computation that may return a value at some point
-- Actually the delay monad
data MayTerminate a = Return a | T (MayTerminate a) deriving Show


instance Functor MayTerminate where
  fmap f (T l)      = fmap f l
  fmap f (Return a) = Return (f a)
  
instance Applicative MayTerminate where
  pure a = Return a
  (T lf) <*> (T lv) = T (lf <*> lv)
  rf     <*> (T lv) = T (rf <*> lv)
  (T lf) <*> rv     = T (lf <*> rv)
  (Return f) <*> (Return v) = Return (f v)
  
instance Monad MayTerminate where
  -- MayTerminate a -> (a -> MayTerminate b) -> MayTerminate b
  (T lv) >>= f = T (lv >>= f)
  (Return v) >>= f = f v


fib :: Int -> MayTerminate Int
fib 0 = Return 1
fib 1 = Return 1
fib n = (+) <$> fib (n - 1) <*> fib (n - 2)

-- Tries to iterate a recursive computation once
oneIteration :: MayTerminate a -> MayTerminate a
oneIteration (Return a) = Return a
oneIteration (T la) = la


compM :: Applicative m => (a -> b, m (a -> b)) -> (a, m a) -> (b, m b)
compM (f, phi) (a, s) = (f a, phi <*> s)

compMcomp :: Applicative m => (x -> (a -> b, m (a -> b))) -> x -> (a, m a) -> (b, m b)
compMcomp = (compM .)


class Bifunctor p => Biapplicative p where
  bipure :: a -> b -> p a b
  (<<*>>) :: p (a -> b) (c -> d) -> p a c -> p b d
  

data Pair f g a b = Pair (f a) (g b)

  
instance (Functor f, Functor g) => Bifunctor (Pair f g) where
  bimap f g (Pair fa gb) = Pair (fmap f fa) (fmap g gb)
  
  
instance (Applicative f, Applicative g) => Biapplicative (Pair f g) where
  bipure a b = Pair (pure a) (pure b)
  (Pair ff gg) <<*>> (Pair fa gb) = Pair (ff <*> fa) (gg <*> gb)

