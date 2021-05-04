{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}

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

module ComonadicStream where

import Control.Comonad
import Control.Comonad.Store
import MonStreams
import PureStreams
import Combinators
import Operations

headC :: Comonad w => MonStr w a -> a
headCM (MCons s) = fst (extract s)

tailC :: Comonad w => MonStr w a -> MonStr w a
tailC (MCons s) = snd (extract s)

takeC :: Comonad w => Int -> MonStr w a -> [a]
takeC 0 s = []
takeC n s = headCMS s : takeCMS (n-1) (tailCMS s)

-- | Generates a comonadic stream using a coKleisli arrow and a starting environment
-- as a seed
iterateC :: Comonad w => (w a -> a) -> w a -> MonStr w a
iterateC f wa = MCons $ fmap (\a -> (a, iterateCMS f (f <<= wa))) wa

{-
  You can think of a comonster as a history of states of the environment in the underlying comonad
-}


extractAllC :: Comonad w => (w a -> b) -> MonStr w a -> Stream b
extractAllC f ws = MCons $ Identity (f (head ws), extractAllC f (extract (tail ws)))


-- Appending to a comonadic stream 
(<@:) :: Comonad w => a -> MonStr w a -> MonStr w a
a <@: (MCons ws) = MCons $ fmap (\wa -> (a, MCons wa)) $ duplicate ws

(!@!) :: Comonad w => MonStr w a -> Int -> a
ws !@! 0 = headCMS ws
ws !@! n = tailCMS ws !@! (n - 1)





-- Testing comonad laws using the store comonad as the underlying functor
{-
instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s
-}

type StoreStream s a = MonStr (Store s) a

{-

 Store comonad is like a container, where s is the index in the container, and the function s -> a gives 
 access to an element in that container, by using the index. 

 If the index s was Int, the container would be an infinite bidirectional stream. If it was Nat, the container
 would be a Stream

-}

runStoreStream :: StoreStream s a -> Stream a
runStoreStream (MCons ss) = let (f, s) = runStore ss
                                (h, t) = f s 
                                in h <: runStoreStream t
                                
intStoreStream :: Int -> StoreStream Int Int
intStoreStream n = MCons $ store (\x -> (n, intStoreStream (n+1))) n

makeStoreStream :: (Int -> b) -> Int -> (Int -> Int) -> StoreStream Int b
makeStoreStream f n g = MCons $ store (\s -> (f s, makeStoreStream f (g n) g)) n

highestFactorSS :: Int -> StoreStream Int Int
highestFactorSS n = makeStoreStream (\x -> maximum [y | y <- [1..x-1], x `mod` y == 0]) n (+1)

biggestPrimeUpToSS :: Int -> StoreStream Int Int
biggestPrimeUpToSS n = makeStoreStream (\x -> maximum [y | y <- [1..x], Prelude.length [z | z <- [1..y], y `mod` z == 0] == 2]) (n + 2) (+1)

basicSS :: Int -> StoreStream Int Int
basicSS n = makeStoreStream (\x -> x+1) n (+4)

x = biggestPrimeUpToSS 5

x1 = extract . duplicate $ x
x2 = fmap extract . duplicate $ x

x3a x = fmap duplicate . duplicate $ x
x3b x = duplicate . duplicate $ x


-- 

data U a = U [a] a [a]

data U2 a = U2 (U (U a))

left :: U a -> U a
left (U (l:ls) x rs) = U ls l (x:rs)

right :: U a -> U a
right (U ls x (r:rs)) = U (x:ls) r rs 

instance Functor U where
  fmap f (U ls x rs) = U (map f ls) (f x) (map f rs)
  
instance Comonad U where
  extract (U _ x _) = x
  duplicate u = U (tail $ iterate left u) u (tail $ iterate right u)
  
 
instance Functor U2 where
  fmap f (U2 u) = U2 (fmap (fmap f) u)
  
instance Comonad U2 where
  extract (U2 u) = extract . extract $ u
  duplicate (U2 u) = fmap U2 $ U2 $ roll $ roll u 
                     where iterate1 f = tail . iterate f
                           roll a = U (iterate1 (fmap left) a) a (iterate1 (fmap right) a)

up :: U2 a -> U2 a
up (U2 a) = U2 (fmap left a)

down :: U2 a -> U2 a
down (U2 a) = U2 (fmap right a)

left2 :: U2 a -> U2 a
left2 (U2 a) = U2 (left a)

right2 :: U2 a -> U2 a
right2 (U2 a) = U2 (right a)

set :: a -> U2 a -> U2 a
set a (U2 (U ls (U ls' x rs') rs)) = U2 (U ls (U ls' a rs') rs)

moveX :: Int -> U2 a -> U2 a
moveX x u2 = if x >= 0 then (iterate left2 u2) !! x  else (iterate right2 u2) !! (abs x)

moveY :: Int -> U2 a -> U2 a
moveY y u2 = if y >= 0 then (iterate down u2) !! y else (iterate up u2) !! (abs y)


rule :: U2 Bool -> Bool
rule (U2 (U
         (U (u0:_) u1 (u2:_):_)
         (U (u3:_) u4 (u5:_))
         (U (u6:_) u7 (u8:_):_))) =
         let n = length $ filter id [u0,u1,u2,u3,u5,u6,u7,u8] in
            u4 && (n==2 || n==3) || (not u4) && n==3
            
zs :: [Bool]
zs = repeat False
            
pattern1 :: U2 Bool
pattern1 = U2 (U
              ((U (False:zs) True (False:zs)):(repeat (U zs False zs)))
               (U (False:zs) False (True:zs))
              ((U (True:zs) True (True:zs)):(repeat (U zs False zs))))

shift i u = (iterate (if i<0 then left else right) u) !! abs i

toList' :: Int -> Int -> U a -> [a]
toList' i j u = take (j-i) $ half $ shift i u 
                where half (U _ b c) = [b] ++ c

toBoard :: Int -> Int -> U2 Bool -> [[Char]]         
toBoard i j (U2 u) = toList' i j (fmap (map (\x -> if x then '#' else ' ') . toList' i j) u) 

rule' (U (a:_) b (c:_)) = not (a && b && not c || (a==b))

main = let u = pattern1
       in (sequence $ fmap (\x -> putStrLn x >> getChar) . fmap unlines $
          map (toBoard (-10) 10) $
          iterate (=>> rule) u) >> return ()
          
          
          

mstrength :: Monad m => m a -> b -> m (a, b)
mstrength ma b = ma >>= (\a -> return (a, b))

type LifeStream a = MonStr U2 a

ls :: LifeStream Bool
ls = iterateCMS rule pattern1

iLs :: MonStr IO (MonStr U2 Bool)
iLs = fmap (iterateCMS rule) (iterateM newCell pattern1)

setCell :: Int -> Int -> Bool -> U2 Bool -> U2 Bool
setCell x y b = moveY (-y) . moveX (-x) . set b . moveX x . moveY y

lifeToScreen :: Int -> Int -> U2 Bool -> IO ()
lifeToScreen n m u2 = do putStrLn (unlines $ toBoard n m u2)
                               
newCell :: U2 Bool -> IO (U2 Bool)
newCell u2 = do putStrLn "Input x, y coord to turn on"
                x <- getLine >>= (readIO :: String -> IO Int)
                y <- getLine >>= (readIO :: String -> IO Int) 
                return $ setCell x y True u2

lsShow :: MonStr IO ()
lsShow = rebaseW (lifeToScreen (-20) 20) ls

ilsShow :: MonStr IO (MonStr IO ())
ilsShow = fmap (rebaseW (lifeToScreen (-20) 20)) iLs


takeInner :: Monad m => Int -> MonStr m (MonStr m a) -> MonStr m a
takeInner n = absorbMS . (\x -> x !!! n)



rebaseW :: (Comonad w, Functor n) => (w a -> n b) -> MonStr w a -> MonStr n b
rebaseW f (MCons ma) = MCons $ fmap (\(h, t) -> (h, rebaseW f t)) (rebaseWaux f ma)

rebaseWaux :: (Comonad w, Functor n) => (w a -> n b) -> forall c. w (a, c) -> n (b, c)
rebaseWaux f wac = fmap (\a -> (a, snd (extract wac))) $ f (fmap fst wac)

iterateM :: Monad m => (a -> m a) -> a -> MonStr m a
iterateM f a = MCons $ fmap (\a -> (a, iterateM f a)) (f a)

-- Not sure how much I trust these functions
rebaseM :: (Monad m, Monad n) => (forall a b. (m a -> n b)) -> MonStr m a -> MonStr n b
rebaseM f (MCons ma) = MCons $ fmap (\(h, t) -> (h, rebaseM f t)) (rebaseMaux f ma)

rebaseMaux :: (Monad m, Monad n) => (forall a b. (m a -> n b)) -> forall c. m (a, c) -> n (b, c)
rebaseMaux f mac = pairA (f (fmap fst mac)) (f (fmap snd mac))


-- runVoidProcess lsShow

type Process a = MonStr IO a

runVoidProcess :: Process () -> IO ()
runVoidProcess (MCons s) = do (a,s') <- s
                              runVoidProcess s'
                              


