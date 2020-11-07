import Control.Concurrent
import Data.Char
import System.IO
import MonStreams
import Operations
import Combinators
import MonStrExamples
import Control.Monad.State

-- compile with: ghc Breakout.hs -i"../" -o breakout -package "mtl-2.2.2" -O2

-- Took the ArrowKey parsing from https://github.com/jhod0/Haskell-Mastermind

data ArrowKey = U | D | L | R deriving Show

-- Parses an Arrow key from stdin
-- If arrow key pressed: IO (Just ArrowKey)
-- If no arrow pressed:  IO Nothing
getArrow :: IO (Maybe ArrowKey)
getArrow = do c <- getChar
              case c of
                 '\ESC' -> parseArrow
                 _      -> return Nothing

parseArrow :: IO (Maybe ArrowKey)
parseArrow = do c <- getChar
                case c of 
                   '\ESC' -> parseArrow
                   '['    -> do l <- getChar
                                case l of 
                                   'A' -> return $ Just U
                                   'B' -> return $ Just D
                                   'C' -> return $ Just R
                                   'D' -> return $ Just L
                                   _   -> return Nothing
                   _      -> return Nothing
   
   
   
-- Found on Haskell wikibooks, inspired the function below                
compete :: [IO a] -> IO a
compete actions = do
    mvar <- newEmptyMVar
    tids <- mapM (\action -> forkIO $ action >>= putMVar mvar) actions
    result <- takeMVar mvar
    mapM_ killThread tids
    return result

-- This waits for the first IO action in the list to execute, and then checks if the others
--  have finished - if any of the others finish, then the return value from the first one
--  to finish is returned, otherwise it returns the value from the "waited for" action
-- This is useful to implement a clock/delay while still allowing the user to input keys
waitForFirst :: [IO a] -> IO a
waitForFirst (wa:actions) = do
    mvar <- newEmptyMVar
    wamvar <- newEmptyMVar
    tids <- mapM (\action -> forkIO $ action >>= putMVar mvar) actions
    watid <- forkIO (wa >>= putMVar wamvar)
    let tids' = (watid:tids) in
       do wax <- takeMVar wamvar
          b <- isEmptyMVar mvar
          x <- if b then (return wax) else takeMVar mvar
          mapM_ killThread tids'
          return x
    
timeout :: Int -> IO a -> a -> IO a
timeout usec action x = compete [action, threadDelay usec >> return x]
                                                                           
maxtime :: Int -> IO a -> a -> IO a
maxtime usec action x = waitForFirst [threadDelay usec >> return x, action]

userInp :: IO Int
userInp = do c <- getChar
             return (ord c)

game :: MonStr (StateT String IO) ()
game = MCons $ do line <- get
                  arrow <- lift $ maxtime 100 getArrow Nothing
                  let ln@(l,ast:r) = break (=='*') line in
                     do (case arrow of
                            Nothing -> return ()
                            Just a -> case a of
                                         L -> case l of 
                                                 [] -> return ()
                                                 xs -> put (init l ++ ast:(last l):r)
                                         R -> case r of 
                                                 []     -> return ()
                                                 (x:xs) -> put (l ++ x:ast:xs)
                                         _ -> return ())
                        out <- get
                        lift $ putStr "\r"
                        lift $ putStr out
                        return ((), game)

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          hSetEcho stdin False
          putStr "            *             "
          runVoidProcess (compileST game "            *             ")

          
