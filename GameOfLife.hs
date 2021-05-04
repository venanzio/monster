{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types #-}

module Demonstration.GameOfLife where

import MonadicStreams hiding (iterate, (++), (!!), head, tail)
import qualified MonadicStreams as MS (iterate, (!!), head, tail)

import ComonadicStreams

import Control.Concurrent
import Examples.Processes 

-- | Redfinition of the store comonad with memoisation

import Data.MemoTrie

data Store s a = Store (s -> a) s

store :: (s -> a) -> s -> Store s a
store = Store

runStore :: Store s a -> (s -> a, s)
runStore (Store f s) = (f, s)

instance HasTrie s => Functor (Store s) where
  fmap f (Store g s) = Store (memo $ f . g) s

instance HasTrie s => Comonad (Store s) where
  extract (Store f a) = f a
  duplicate (Store f s) = Store (Store f) s
  
  
-- | Game of life demonstration using the Store comonad

type Grid a = Store (Int, Int) a

type LifeStream a = MonStr (Store (Int, Int)) a

adjacentAlive :: Grid Bool -> Int
adjacentAlive str = let (f, (x, y)) = runStore str in sum [1 | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1], (x /= x' || y /= y') && f (x', y')]

rule :: Grid Bool -> Bool
rule str = case adjacentAlive str of
              2 -> extract str
              3 -> True
              _ -> False

glider :: (Int, Int) -> Bool
glider (9 , 9 ) = True
glider (10, 9 ) = True
glider (11, 9 ) = True
glider (11, 10) = True
glider (10, 11) = True
glider _        = False

lifeGrid :: Grid Bool
lifeGrid = store glider (0, 0)

toBoard :: Int -> Int -> Grid Bool -> [[Char]]
toBoard w h str = let (f, _) = runStore str in [[if f (x, y) then '#' else ' ' | x <- [0 .. w]] | y <- [0 .. h]]

displayLife :: Int -> Int -> Grid Bool -> IO ()
displayLife w h str = do putStr "\ESC[2J" 
                         putStrLn . unlines . toBoard w h $ str

setCell :: Bool -> Int -> Int -> Grid Bool -> Grid Bool
setCell b x y str = let (f, fcs) = runStore str in store (\(x', y') -> if x' == x && y' == y then b else f (x', y')) fcs

newCell :: Grid Bool -> IO (Grid Bool)
newCell str = do putStrLn "Input x, y coord to turn on"
                 x <- getLine >>= (readIO :: String -> IO Int)
                 y <- getLine >>= (readIO :: String -> IO Int) 
                 return $ setCell True x y str

lifeStreamDemo :: LifeStream Bool
lifeStreamDemo = iterateC rule lifeGrid

interactiveLifeStreams :: MonStr IO (LifeStream Bool)
interactiveLifeStreams = fmap (iterateC rule) (MS.iterate newCell lifeGrid)

rebaseW :: (Comonad w, Functor n) => (w a -> n b) -> MonStr w a -> MonStr n b
rebaseW f (MCons ma) = MCons $ fmap (\(h, t) -> (h, rebaseW f t)) (rebaseWaux f ma)

rebaseWaux :: (Comonad w, Functor n) => (w a -> n b) -> forall c. w (a, c) -> n (b, c)
rebaseWaux f wac = fmap (\a -> (a, snd (extract wac))) $ f (fmap fst wac)

iLSShow :: MonStr IO (MonStr IO ())
iLSShow = fmap (rebaseW (displayLife 20 20)) interactiveLifeStreams

-- | Running the streams

run = runVoidProcess (rebaseW (displayLife 20 20) lifeStreamDemo)

runInteractive = runVoidProcess (takeInnerM 2 iLSShow)
                         
-- | Running with delays

waitProc :: Process ()
waitProc = MS.iterate (const (threadDelay 250000)) () -- sleeps for a quarter second

runDelay = runVoidProcess $ interleaveActM (rebaseW (displayLife 20 20) lifeStreamDemo) waitProc