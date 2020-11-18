import Control.Concurrent
import Data.Char
import System.IO
import MonStreams
import Operations
import Combinators
import MonStrExamples
import Control.Monad.State
import Graphics.HGL

-- compile with: ghc Breakout.hs -i"../" -o breakout -package "mtl-2.2.2" -O2

-- Took the ArrowKey parsing method from https://github.com/jhod0/Haskell-Mastermind

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
   
   
   
-- Found on Haskell wikibooks, informed the waitForFirst function below                
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

screenWidth :: Int 
screenWidth = 80

screenHeight :: Int
screenHeight = 24

ballSpeed :: Double
ballSpeed = 0.5
             
             
type GameState = (Int, (Double, Double), (Double, Double))

movePaddle :: StateT GameState IO ()
movePaddle = do (batp, ballp, ballv) <- get
                arrow <- lift $ maxtime 100000 getArrow Nothing
                case arrow of
                   Nothing -> return ()
                   Just a -> case a of
                                L -> if batp == 0 then return () else put ((batp - 1), ballp, ballv)
                                R -> if batp == 73 then return () else put ((batp + 1), ballp, ballv)
                                _ -> return ()
               
constrainBallY :: (Double,Double) -> (Double, Double) -> ((Double,Double),(Double, Double))
constrainBallY (bx,by) (bvx,bvy) = if by <= 1.0 then ((bx,1.0),(bvx,-bvy))
                                              else if by >= fromIntegral screenHeight then (((fromIntegral screenWidth) / 2, 3.0), (0,1))
                                                                         else ((bx,by),(bvx,bvy))
                           
constrainBall :: (Double,Double) -> (Double, Double) -> ((Double,Double),(Double, Double))
constrainBall (bx,by) (bvx,bvy) = if bx <= 0 then constrainBallY (0,by) (-bvx,bvy)
                                             else if bx >= fromIntegral screenWidth then constrainBallY ((fromIntegral screenWidth)-0.1,by) (-bvx,bvy)
                                                                       else constrainBallY (bx,by) (bvx,bvy)
                           
batCollision :: GameState -> GameState
batCollision r@(batp, (bx,by), (bvx,bvy)) = if (round by == 22) && (signum(bvy) > 0) then 
                                               (if bx >= fromIntegral (batp-1) && bx < fromIntegral (batp+7) then (batp, (bx,by), (((2.0/7.0) * (bx - fromIntegral batp)) - 1,-bvy))
                                                                                                         else r) 
                                                                                     else r
                           
moveBall :: StateT GameState IO ()
moveBall = do (batp, ballp, ballv) <- get 
              let ((bx,by), (bvx,bvy)) = constrainBall ballp ballv 
                  (batp', (bx',by'), (bvx',bvy')) = batCollision (batp, (bx,by), (bvx,bvy)) in
                  put (batp', (bx' + (ballSpeed * bvx'), by' + (ballSpeed * bvy')), (bvx',bvy'))

emptyLine :: String
emptyLine = (replicate 80 ' ') ++ ['\n']

emptyLines :: Int -> String
emptyLines n = concat (replicate n emptyLine)
                       
printGame :: StateT GameState IO ()
printGame = do (batp, ballp, ballv) <- get
               -- printing the ball, y in [0,24), x in [0,80), y = 0 is the top of the screen
               (let (bx,by) = (\(x,y) -> (round x, round y)) ballp in
                   do lift $ putStr (emptyLines (by-1))
                      lift $ putStr $ (replicate bx ' ') ++ '*':(replicate ((screenWidth-1) - bx) ' ' ++ ['\n'])
                      lift $ putStr (emptyLines ((screenHeight-3)-(by-1)))
                   )
               -- printing bat
               lift $ putStr $ (replicate batp ' ') ++ "=======" ++ (replicate ((screenWidth-8) - batp) ' ')
               lift $ putStr (replicate 2 '\n')

-- Using combinators and streamify to turn the actions into a continuous process          
game :: MonStr (StateT GameState IO) ()
game = (streamify movePaddle) |:> moveBall |:> printGame

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          hSetEcho stdin False
          runVoidProcess (compileST game (screenWidth `div` 2,((fromIntegral screenWidth/2),3.0),(0,1.0)))

          
