import MonadicStreams hiding (head,tail,repeat)
import qualified MonadicStreams as M

import Control.Monad.Identity
import System.Random

-- A few examples of application of monster operations

nats :: Applicative m => MonStr m Int
nats = natsFrom 0 where
  natsFrom n = n <: natsFrom (n+1)

inputStr :: Read a => MonStr IO a
inputStr = M.repeat (putStr("input: ") >> fmap read getLine)
{-
inputStr = MCons $ do
  putStr("input: ")
  s <- getLine
  return (read s, inputStr)
-}

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

-- Alternative definitions of nats
--   Are they equivalent to nats for every m?

myNats :: Applicative m => MonStr m Int
myNats = 0 <: accumulate (repeatA 1)

appNats :: Applicative m => MonStr m Int
appNats = 0 <: (pure (+ 1) <*> appNats)

-- Guessing game
-- you can use it with nats or inputStr

guessNum :: Int -> MonStr IO Int -> IO ()
guessNum x s = do
  putStrLn "Guess the number"
  (y,s') <- uncons s
  putStr (show y Prelude.++ " is ")
  if y == x
    then putStrLn "correct" >> return ()
    else putStrLn (if y < x then "too small" else "too big") >> guessNum x s'

guessGame :: MonStr IO Int -> IO ()
guessGame s = do
  x <- randomRIO (0,100)
  guessNum x s
