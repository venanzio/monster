import MonadicStreams hiding (head,tail)
import qualified MonadicStreams as M

import Control.Monad.Identity

-- A few examples of application of monster operations

nats :: Applicative m => MonStr m Int
nats = natsFrom 0 where
  natsFrom n = n <: natsFrom (n+1)

echo :: Read a => MonStr IO a
echo = MCons $ do
  putStr("input: ")
  s <- getLine
  return (read s, echo)


-- print the first 20 elements of a polymorphic monster
--  you can apply it to a monster of type: Monad m => Monstr m a
--    (or any monster that can be instantiated to Identity)

showMonster :: MonStr Identity a -> [a]
showMonster = runIdentity . takeM 20


-- Initial segment before the first occurrence of 0

stopAtZero :: Monad m => MonStr m Int -> m [Int]
stopAtZero = fmap fst . spanM (/=0)



-- stream of partial sums
accumulate :: Functor m => MonStr m Int -> MonStr m Int
accumulate = accFrom 0 where
  accFrom s = transform (\h t -> (h+s, accFrom (h+s) t))

myNats :: Applicative m => MonStr m Int
myNats = 0 <: accumulate (repeatA 1)

-- Q: is myNats equivalent to nats for every m?

-- ex: runIdentity (takeM 10 myNats)
