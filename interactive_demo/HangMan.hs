module Main where
 
import MonStreams
import PureStreams
import Operations
import MonStrExamples
import Combinators
import Control.Monad.State
 
main :: IO ()
main = playHangman

playHangman :: IO ()
playHangman = do putStrLn "Pick a word:"
                 s <- getLine
                 runHangman s
                 return ()

runHangman :: String -> IO [Bool]
runHangman s = stopAtPred (== False) (compileST (game $:> traceGame $:> lift endEarly) [(c,False) | c <- s])
 
game :: MonStr (StateT ([(Char, Bool)]) IO) Bool
game = MCons $ do cs <- get
                  lift $ putStrLn ("Word: " ++ fmap mask cs)
                  lift $ putStrLn "Guess a letter:"
                  guess <- lift getChar
                  lift $ putStrLn ""
                  let cs' = [ (c, m || c == guess) | (c,m) <- cs ] in
                     do put cs'
                        if and (fmap snd cs') then do lift (putStrLn ("You win! Word is " ++ fmap fst cs)) 
                                                      return (False, game)
                                              else return (True, game)

traceGame :: StateT ([(Char, Bool)]) IO (Bool -> Bool)
traceGame = do cs <- get
               let count = sum [1 | (c,b) <- cs, b] in
                  do lift $ putStrLn ("You've currently revealed: " ++ show count ++ " letters")
                     return (\b -> if count == length cs then (b && False) else (b && True))

endEarly :: IO (Bool -> Bool)
endEarly = do putStrLn "End early (y/n): "
              c <- getChar
              putStrLn ""
              if c == 'y' then return (&& False)
                          else return (&& True)

mask :: (Char, Bool) -> Char
mask (c, True)  = c
mask (c, False) = '-'