{-# LANGUAGE FlexibleInstances #-}

module Examples.PureStreams where
 
import Prelude hiding (head, tail, (++))
import qualified Prelude as P ((++))
  
import MonadicStreams
import Control.Monad.Identity

type Stream = MonStr Identity

-- | Show instance for pure streams just displays the first
-- 5 elements, since any pure stream is infinite. This is 
-- mostly for demonstration and debugging purposes
instance Show a => Show (Stream a) where
  show s = foldr (\a b -> (show a) P.++ " <: " P.++ b) "..." (takeS 5 s)

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


-- * Examples of pure streams

fromN :: Int -> Stream Int
fromN n = n <: fromN (n+1)

nats :: Stream Int
nats = fromN 0