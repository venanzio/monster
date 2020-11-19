import System.IO
import MonStreams
import Operations
import Combinators
import MonStrExamples
import Control.Monad.State
import Graphics.HGL

{-

To compile, use: ghc BreakoutHGL.hs -o breakoutHGL -i"../" (to reference the library module in the level above)

Dependancies: HGL-3.2.3.2 (cabal install HGL)

-}

type GameState = (Int, (Double, Double), (Double, Double), Window)

data ArrowKey = U | D | L | R deriving Show

screenWidth :: Int 
screenWidth = 80

screenHeight :: Int
screenHeight = 24

ballSpeed :: Double
ballSpeed = 0.05

getMaybeKeyPress :: Window -> IO (Maybe ArrowKey)
getMaybeKeyPress w = do e <- maybeGetWindowEvent w
                        return (case e of 
                                   (Just (Key k d)) | isLeftKey  k -> Just L
                                                    | isRightKey k -> Just R
                                                    | isUpKey    k -> Just U
                                                    | isDownKey  k -> Just D
                                                    | otherwise    -> Nothing
                                   _                -> Nothing
                                )

movePaddle :: StateT GameState IO ()
movePaddle = do (batp, ballp, ballv, w) <- get
                (arrow,_) <- lift $ par (getMaybeKeyPress w) (getWindowTick w)
                case arrow of
                   Nothing -> return ()
                   Just a -> case a of
                                L -> if batp == 0 then return () else put ((batp - 1), ballp, ballv, w)
                                R -> if batp == 73 then return () else put ((batp + 1), ballp, ballv, w)
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
batCollision r@(batp, (bx,by), (bvx,bvy), w) = if (round by == 22) && (signum(bvy) > 0) then 
                                                 (if bx >= fromIntegral (batp-1) && bx < fromIntegral (batp+7) then (batp, (bx,by), (((2.0/7.0) * (bx - fromIntegral batp)) - 1,-bvy),w)
                                                                                                           else r) 
                                                                                       else r
                           
moveBall :: StateT GameState IO ()
moveBall = do (batp, ballp, ballv, w) <- get 
              let ((bx,by), (bvx,bvy)) = constrainBall ballp ballv 
                  (batp', (bx',by'), (bvx',bvy'), _) = batCollision (batp, (bx,by), (bvx,bvy), w) in
                  put (batp', (bx' + (ballSpeed * bvx'), by' + (ballSpeed * bvy')), (bvx',bvy'), w)
                  
printGame :: StateT GameState IO ()
printGame = do (batp, ballp, ballv, w) <- get
               lift $ clearWindow w
               -- printing the ball, y in [0,24), x in [0,80), y = 0 is the top of the screen
               (let (bx,by) = (\(x,y) -> (round x, round y)) ballp in
                   lift $ drawInWindow w (ellipse (10 * bx - 5, 10 * by - 5) (10 * bx + 5, 10 * by + 5))
                   )
               -- printing bat
               lift $ drawInWindow w (polygon [(10*batp, 220),(10*batp,230),(10*(batp+7),230),(10*(batp+7),220)])

-- Using combinators and streamify to turn the actions into a continuous process          
game :: MonStr (StateT GameState IO) ()
game = (streamify movePaddle) |:> moveBall |:> printGame

main :: IO ()
main = runGraphics $
       do w <- openWindowEx "Breakout" Nothing (screenWidth * 10, screenHeight * 10) DoubleBuffered (Just 1)
          runVoidProcess (compileST game (screenWidth `div` 2,((fromIntegral screenWidth/2),3.0),(0,1.0), w))
          closeWindow w
       
       