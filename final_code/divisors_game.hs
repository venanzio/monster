import MonadicStreams hiding ((++))
-- import qualified MonadicStreams as M

import Examples.BLTrees

-- Divisors game

divides :: Int -> Int -> Bool
divides n m = m `mod` n == 0

divisors :: Int -> [Int]
divisors n = filter (\x -> divides x n) [1..n-1]

elimDivisors :: Int -> [Int] -> [Int]
elimDivisors m = filter (\x -> not (divides x m))

type GameState = (Bool, [Int])

printPlayer :: Bool -> String
printPlayer True = "Player I"
printPlayer False = "Player II"

move :: [Int] -> IO Int
move board = do
  putStrLn "Choose an element in the board:"
  s <- getLine
  let n = read s
  if n `elem` board
    then return n 
    else putStrLn "That number is not in the board" >> move board

playGame :: GameState -> IO ()
playGame (player, board) =
  if board == []
    then putStrLn ((printPlayer (not player)) ++ " wins!")
    else do
      putStrLn ((printPlayer player) ++ " to move.")
      putStrLn ("Board: " ++ (show board))
      n <- move board
      playGame (not player, elimDivisors n board)
         

play :: Int -> IO ()
play n = playGame (True,divisors n)

-- Playing the game as a monster

gameIOM :: GameState -> MonStr IO Int
gameIOM (player, board) = MCons $
    if board == []
    then do
      putStrLn ((printPlayer (not player)) Prelude.++ " wins!")
      return (0, (repeatA 0))   
    else do
      putStrLn ((printPlayer player) Prelude.++ " to move.")
      putStrLn ("Board: " Prelude.++ (show board))
      n <- move board
      return (n, gameIOM (not player, elimDivisors n board))

-- non-zero prefix of a monster
nonz :: Monad m => MonStr m Int -> m [Int]
nonz m = fmap fst $ spanM (/= 0) m

playMonster :: Int -> IO [Int]
playMonster n = nonz (gameIOM (True,divisors n))


-- The whole game tree as a monster

moves :: GameState -> [Int]
moves (_,board) = board

mkMove :: GameState -> Int -> GameState
mkMove (player,board) n = (not player, elimDivisors n board)

gameTree :: GameState -> BLTree Int
gameTree g = MCons $ do
  n <- moves g
  return (n, gameTree (mkMove g n))

showGameTree :: Int -> IO ()
showGameTree n = putStrLn (showTree (gameTree (True, divisors n)))



-- Trying to do it in abstract

type AbstMove m = GameState -> m Int

gameMonster :: Monad m => AbstMove m -> GameState -> MonStr m Int
gameMonster am g = MCons $ do
  n <- am g
  return (n, gameMonster am (mkMove g n))

endPos :: GameState -> Bool
endPos (player,board) = board==[]

moveIO :: AbstMove IO
moveIO (player,board) =
  if board == []
    then do
      putStrLn ((printPlayer (not player)) Prelude.++ " wins!")
      return 0
    else do
      putStrLn ((printPlayer player) Prelude.++ " to move.")
      putStrLn ("Board: " Prelude.++ (show board))
      move board

-- Versions of gameTree and gameIOM

gTree :: GameState -> BLTree Int
gTree = gameMonster moves

gIOM :: GameState -> MonStr IO Int
gIOM = gameMonster moveIO



showGTree :: Int -> IO ()
showGTree n = putStrLn (showTree (gTree (True, divisors n)))

playGIOM :: Int -> IO [Int]
playGIOM n = nonz (gIOM (True,divisors n))
