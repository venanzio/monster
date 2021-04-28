module Examples.PureStreams where
 
import Prelude hiding (head, tail, map, scanl, scanl1,
  iterate, take, drop, takeWhile, (++),
  dropWhile, repeat, cycle, filter, (!!), 
  zip, unzip, zipWith, zip3, unzip3, zipWith3,
  words,unwords,lines,unlines, break, span, splitAt)
  
import MonadicStreams
import Control.Monad.Identity

type Stream = MonStr Identity

headS :: Stream a -> a
headS = runIdentity . head

tailS :: Stream a -> Stream a
tailS = runIdentity . tail

outS :: Stream a -> (a, Stream a)
outS s = (headS s, tailS s) 

takeS :: Int -> Stream a -> [a]
takeS n s
  | n <= 0    = []
  | otherwise = headS s : takeS (n-1) (tailS s) 

dropS :: Int -> Stream a -> Stream a
dropS n s
  | n <= 0    = s
  | otherwise = dropS (n-1) (tailS s)

mapS :: (a -> b) -> Stream a -> Stream b
mapS f s = fmap f s

(!|) :: Stream a -> Int -> a
s !| 0 = headS s
s !| n = (tailS s) !| (n - 1)


-- | Examples of pure streams

nats :: Stream Int
nats = fromNat 0
       where fromNat n = n <: fromNat (n+1)