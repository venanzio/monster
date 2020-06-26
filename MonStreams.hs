-- Monadic Streams
--   Venanzio Capretta, 2020


-- Type of monadic streams
-- f is not required to be a monad

data Str a = Cons a (Str a)

data MonStr m a = MCons (m (a , MonStr m a))

-- Example: Lazy lists - Maybe Monad
------------------------------------

type LList a = MonStr Maybe a

nil :: LList a
nil = MCons Nothing

cons :: a -> LList a -> LList a
cons a l = MCons (Just (a,l))

llist :: [a] -> LList a
llist = foldr cons nil

fromL :: LList a -> [a]
fromL (MCons Nothing) = []
fromL (MCons (Just (a,l))) = a : fromL l

-- Arbitrarily branching trees - List Monad
-------------------------------------------

type Tree a = MonStr [] a

leaf :: Tree a
leaf = MCons []

node :: [(a,Tree a)] -> Tree a
node l = MCons l

-- Depth-first traversal
dfLabels :: Tree a -> [a]
dfLabels (MCons l) =  concat (map (\(a,l') -> a:dfLabels l') l)

-- Breath-first traversal
bfLabels :: Tree a -> [a]
bfLabels t = bfLabs [t]

bfLabs :: [Tree a] -> [a]
bfLabs [] = []
bfLabs ((MCons l):ts) = map fst l ++ bfLabs (ts ++ map snd l)


t1 = node [(5,leaf),
           (3, node [(1,leaf)]),
           (2, node [])
          ]

-- Interactive Processes -- IO Monad
------------------------------------

type Process a = MonStr IO a

runProcess :: Process a -> IO [a]
runProcess (MCons s) = do
    (a,s') <- s
    as <- runProcess s'
    return (a:as)

-- A stream that adds up the inputs from the user
-- Prints the partial sums

sumProc :: Int -> Process Int
sumProc n = MCons $ do
  putStrLn ("sum so far: "++(show n))
  s <- getLine
  let n' = n + read s
  return (n, sumProc n')

-- How to use the (infinite) return of a process
--  stop when it is zero

stopAtZero :: Process Int -> IO [Int]
stopAtZero s = do
  as <- runProcess s
  let l = takeWhile (/=0) as 
  return l

-- But stopAtZero (sumProc 0) doesn't stop when the sum reaches 0
--   s not lazily evaluated?
