import MonadicStreams
import Examples.LazyLists
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
