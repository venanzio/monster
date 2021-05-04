{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}

module Demonstration.GameOfLife where

import MonadicStreams hiding (iterate, (++), (!!), head, tail)
import qualified MonadicStreams as MS (iterate, (!!), head, tail)

import ComonadicStreams

import Control.Concurrent
import Examples.Processes 

import Control.Comonad.Store


-- | This example is adapted from one in Neighbourhood of Infinity
-- http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html 


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
ls = iterateC rule pattern1

iLs :: MonStr IO (MonStr U2 Bool)
iLs = fmap (iterateC rule) (MS.iterate newCell pattern1)

setCell :: Int -> Int -> Bool -> U2 Bool -> U2 Bool
setCell x y b = moveY (-y) . moveX (-x) . set b . moveX x . moveY y

lifeToScreen :: Int -> Int -> U2 Bool -> IO ()
lifeToScreen n m u2 = do threadDelay 250000 -- sleep for a quarter second, 0.25 million microseconds
                         putStr "\ESC[2J"
                         putStrLn (unlines $ toBoard n m u2)
                               
newCell :: U2 Bool -> IO (U2 Bool)
newCell u2 = do putStrLn "Input x, y coord to turn on"
                x <- getLine >>= (readIO :: String -> IO Int)
                y <- getLine >>= (readIO :: String -> IO Int) 
                return $ setCell x y True u2

lsShow :: MonStr IO ()
lsShow = rebaseW (lifeToScreen (-20) 20) ls

ilsShow :: MonStr IO (MonStr IO ())
ilsShow = fmap (rebaseW (lifeToScreen (-20) 20)) iLs

rebaseW :: (Comonad w, Functor n) => (w a -> n b) -> MonStr w a -> MonStr n b
rebaseW f (MCons ma) = MCons $ fmap (\(h, t) -> (h, rebaseW f t)) (rebaseWaux f ma)

rebaseWaux :: (Comonad w, Functor n) => (w a -> n b) -> forall c. w (a, c) -> n (b, c)
rebaseWaux f wac = fmap (\a -> (a, snd (extract wac))) $ f (fmap fst wac)


-- | Functions to run the show

run = runVoidProcess lsShow

runInteractive = runVoidProcess (takeInnerM 2 ilsShow)



-- | New example with Store comonad instead

type Grid a = Store (Int, Int) a

type LifeStreamG a = MonStr (Store (Int, Int)) a

-- | Helper to quickly evaluate a store
appStore :: Store a b -> b
appStore str = let (f, s) = runStore str in f s

adjacentAlive :: Grid Bool -> Int
adjacentAlive str = let (f, (x, y)) = runStore str in sum [1 | x' <- [x - 1 .. x + 1], y' <- [y], (x /= x' || y /= y') && f (x', y') ]

ruleG :: Grid Bool -> Bool
ruleG str = case adjacentAlive str of
               2 -> appStore str
               3 -> True
               _ -> False

lifeG :: (Int, Int) -> Bool
lifeG (9 , 9 ) = True
lifeG (10, 9 ) = True
lifeG (11, 9 ) = True
lifeG (11, 10) = True
lifeG (10, 11) = True
lifeG (10, 8 ) = False
lifeG _        = False

lifeGrid :: Grid Bool
lifeGrid = store lifeG (0, 0)

toBoardG :: Int -> Int -> Grid Bool -> [[Char]]
toBoardG w h str = let (f, _) = runStore str in [[if f (x, y) then '#' else ' ' | x <- [0 .. w]] | y <- [0 .. h]]

displayLifeG :: Int -> Int -> Grid Bool -> IO ()
displayLifeG w h = putStrLn . unlines . toBoardG w h

setCellG :: Bool -> Int -> Int -> Grid Bool -> Grid Bool
setCellG b x y str = let (f, fcs) = runStore str in store (\(x', y') -> if x' == x && y' == y then b else f (x', y')) fcs

newCellG :: Grid Bool -> IO (Grid Bool)
newCellG str = do putStrLn "Input x, y coord to turn on"
                  x <- getLine >>= (readIO :: String -> IO Int)
                  y <- getLine >>= (readIO :: String -> IO Int) 
                  return $ setCellG True x y str

waitProc :: Process ()
waitProc = MS.iterate (const (threadDelay 250000)) () -- sleeps for a quarter second

lsG :: LifeStreamG Bool
lsG = iterateC ruleG lifeGrid

lsGShow = rebaseW (displayLifeG 20 20) lsG

oneIter = displayLifeG 20 20 (ruleG <<= lifeGrid)