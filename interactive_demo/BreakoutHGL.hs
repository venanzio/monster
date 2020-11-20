import System.IO
import MonStreams
import Operations
import Combinators
import MonStrExamples
import Control.Monad.State
import Data.List
import Data.Maybe
import Graphics.HGL

{-

To compile, use: ghc BreakoutHGL.hs -o breakoutHGL -i"../" (to reference the library module in the level above)

Dependancies: HGL-3.2.3.2 (cabal install HGL --lib, and then add "package-id HGL-3.2.3.2" to the end of your ghc environment)

-}

type GameState = (Int, (Double, Double), (Double, Double), [(Double,Double)], Window)

data ArrowKey = U | D | L | R deriving Show

screenWidth :: Int 
screenWidth = 800

screenHeight :: Int
screenHeight = 240

ballSpeed :: Double
ballSpeed = 1.0

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
movePaddle = do (batp, ballp, ballv, blockps, w) <- get
                (arrow,_) <- lift $ par (getMaybeKeyPress w) (getWindowTick w)
                case arrow of
                   Nothing -> return ()
                   Just a -> case a of
                                L -> if batp == 0 then return () else put ((batp - 5), ballp, ballv, blockps, w)
                                R -> if batp == 730 then return () else put ((batp + 5), ballp, ballv, blockps, w)
                                _ -> return ()

constrainBallY :: (Double,Double) -> (Double, Double) -> ((Double,Double),(Double, Double))
constrainBallY (bx,by) (bvx,bvy) = if by <= 1.0 then ((bx,1.0),(bvx,-bvy))
                                              else if by >= fromIntegral screenHeight then (((fromIntegral screenWidth) / 2, 3.0), (0,1))
                                                                         else ((bx,by),(bvx,bvy))

constrainBall :: (Double,Double) -> (Double, Double) -> ((Double,Double),(Double, Double))
constrainBall (bx,by) (bvx,bvy) = if bx <= 0 then constrainBallY (0,by) (-bvx,bvy)
                                             else if bx >= fromIntegral screenWidth then constrainBallY ((fromIntegral screenWidth)-0.1,by) (-bvx,bvy)
                                                                                    else constrainBallY (bx,by) (bvx,bvy)

-- Currently doesn't seem to work for bouncing from underneath
calculateBounce :: (Double, Double) -> (Double, Double) -> Double -> Double -> (Double, Double)
calculateBounce (bx,by) (bvx,bvy) cx width = (ballSpeed * sin (a*offset), (-ballSpeed) * cos (a*offset))
                                             where offset = negate $ ((cx + (width/2.0)) - bx)/(cx + (width/2.0))
                                                   a      = 2 * pi

batCollision :: GameState -> GameState
batCollision r@(batp, (bx,by), (bvx,bvy), blockps, w) = if (round by > 220 && round by <= 230) && (signum (bvy) > 0) then 
                                                        (if bx >= fromIntegral (batp-10) && bx < fromIntegral (batp+70) then (batp, (bx,220), calculateBounce (bx,by) (bvx,bvy) (fromIntegral batp) 60.0,blockps,w)
                                                                                                                        else r) 
                                                                                                  else r

blockCollisions :: GameState -> GameState
blockCollisions r@(batp, (bx,by), (bvx,bvy), [], w)          = r
blockCollisions r@(batp, (bx,by), (bvx,bvy), (b:blockps), w) = case singleBlockCollision (bx,by) (bvx,bvy) b of 
                                                                  Just (nxv,nyv) -> (batp, (bx,by), (nxv,nyv), blockps, w)
                                                                  Nothing        -> (\(bp,bap,bav,bs,w) -> (bp,bap,bav,(b:bs),w)) $ blockCollisions (batp, (bx,by), (bvx,bvy), blockps, w)
                                                                  
-- Block positions indicate bottom left corner of blocks, and blocks are 20x10
-- Maybe returns a new ball speed
singleBlockCollision :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Maybe (Double,Double) 
singleBlockCollision (bx,by) (bvx,bvy) (blx, bly) = if (bx > blx - 1 && bx < blx + 21) then
                                                       if (by > bly && by < bly + 10) then Just (calculateBounce (bx,by) (bvx,bvy) blx 20.0)
                                                                                      else Nothing
                                                                                       else Nothing
                                           

moveBall :: StateT GameState IO ()
moveBall = do (batp, ballp, ballv, blockps, w) <- get 
              let ((bx,by), (bvx,bvy)) = constrainBall ballp ballv 
                  -- Interestingly, reassigning w here (where the _ is), breaks the window pointer
                  (batp', (bx',by'), (bvx',bvy'), blockps', _) = (blockCollisions . batCollision) (batp, (bx,by), (bvx,bvy), blockps, w) in
                  put (batp', (bx' + (ballSpeed * bvx'), by' + (ballSpeed * bvy')), (bvx',bvy'), blockps', w)
                  
rectangle :: Point -> Point -> Graphic
rectangle (x,y) (x',y') = polygon [(x, y),(x,y'),(x',y'),(x',y)]
                  
printGame :: StateT GameState IO ()
printGame = do (batp, ballp, ballv, blockps, w) <- get
               lift $ clearWindow w
               -- printing the ball, y in [0,240), x in [0,800), y = 0 is the top of the screen
               (let (bx,by) = (\(x,y) -> (round x, round y)) ballp in
                   lift $ drawInWindow w (ellipse (bx - 5, by - 5) (bx + 5, by + 5))
                   )
               -- printing bat
               lift $ drawInWindow w (rectangle (batp-5, 220) (batp+65,230))
               -- printing blocks
               lift $ sequence_ $ fmap (\(x,y) -> drawInWindow w $ rectangle (round x, round y) (round $ x+20,round $ y+10)) blockps

-- Using combinators and streamify to turn the actions into a continuous process          
game :: MonStr (StateT GameState IO) ()
game = (streamify movePaddle) |:> moveBall |:> printGame

main :: IO ()
main = runGraphics $
       do w <- openWindowEx "Breakout" Nothing (screenWidth, screenHeight) DoubleBuffered (Just 1)
          runVoidProcess (compileST game (screenWidth `div` 2,((fromIntegral screenWidth/2),3.0),(0,1.0), [(10 + x * 30, 50) | x <- [1..20]], w))
          closeWindow w
       
       