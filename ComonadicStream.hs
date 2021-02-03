{-

 As a thought, comonadic streams might have more of an application to functional reactive programming,
  since they model the idea of a potentially infinite stream of data being collapsed by a function, with
  the whole data structure being able to be copied again for further computations (using duplicate)

 This file is used to test the comonad laws with the store (and maybe other) comonad(s), to verify whether 
  monadic streams are indeed comonads (if they are over a comonadic functor)

 https://stackoverflow.com/questions/16551734/can-a-monad-be-a-comonad - see the second answer here

  It seems that monadic streams are ~almost~ equivalent to the cofree functor construction, and hence
  maybe ~some~ things that are true about cofree instanciated with different types of functor are also
  true for monadic streams

  Cofree f a = (a, f (Cofree f a))
  MonStr f a = f (a, MonStr f a)

  This difference *does* seem to be important for the distinction between Rose trees (Cofree []) and 
  List-monsters (MonStr []) - this is discussed further in ListMonsterMonad.hs

-}

import Control.Comonad
import Control.Comonad.Store
import MonStreams

{-
instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s
-}

type StoreStream s a = MonStr (Store s) a

headCMS :: Comonad w => MonStr w a -> a
headCMS (MCons s) = fst (extract s)

tailCMS :: Comonad w => MonStr w a -> MonStr w a
tailCMS (MCons s) = snd (extract s)

takeCMS :: Comonad w => Int -> MonStr w a -> [a]
takeCMS 0 s = []
takeCMS n s = headCMS s : takeCMS (n-1) (tailCMS s)

intStoreStream :: Int -> StoreStream Int Int
intStoreStream n = MCons $ store (\x -> (n, intStoreStream (n+1))) n

makeStoreStream :: (Int -> b) -> Int -> (Int -> Int) -> StoreStream Int b
makeStoreStream f n g = MCons $ store (\s -> (f s, makeStoreStream f (g n) g)) n

highestFactorSS :: Int -> StoreStream Int Int
highestFactorSS n = makeStoreStream (\x -> maximum [y | y <- [1..x-1], x `mod` y == 0]) n (+1)

biggestPrimeUpToSS :: Int -> StoreStream Int Int
biggestPrimeUpToSS n = makeStoreStream (\x -> maximum [y | y <- [1..x], Prelude.length [z | z <- [1..y], y `mod` z == 0] == 2]) n (+1)

basicSS :: Int -> StoreStream Int Int
basicSS n = makeStoreStream (\x -> x+1) n (+4)

x = biggestPrimeUpToSS 5

x1 = extract . duplicate $ x
x2 = fmap extract . duplicate $ x

x3a = fmap duplicate . duplicate $ x
x3b = duplicate . duplicate $ x

