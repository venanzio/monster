module ApplicativeLaws where
 
import Prelude hiding (head, tail)
import qualified Prelude as P ((!!), iterate, head, tail, cycle) 
 
import MonadicStreams hiding ((++))
import Examples.PureStreams

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b) 

instance Applicative Pair where
  pure a = Pair a a
  (Pair f g) <*> (Pair a b) = Pair (f a) (g b)
  
modifyPairs :: (a -> b) -> (a -> b) -> MonStr Pair a -> MonStr Pair b
modifyPairs f g (MCons (Pair (a, as) (b, bs))) = (MCons (Pair (f a, modifyPairs f g as) (g b, modifyPairs f g bs)))

pairStr :: Int -> MonStr Pair Int
pairStr n = MCons (Pair (n, pairStr (n * 2)) (n + 1, pairStr ((n * 2) + 1)))

pairFuncStr :: Int -> MonStr Pair (Int -> Int)
pairFuncStr n = MCons (Pair ((+n), pairFuncStr (n * 2)) ((+(n+1)), pairFuncStr ((n * 2) + 1)))

interchange :: MonStr Pair a -> MonStr Pair ((a -> b) -> b)
interchange = fmap (\a -> (\f -> f a))

checkLaw :: Eq b => MonStr Pair ((a -> b) -> b) -> MonStr Pair a -> MonStr Pair (a -> b) -> Stream Bool
checkLaw maf ma mf = checkAux (mf <*> ma) (maf <*> mf)
           
checkAux :: Eq b => MonStr Pair b -> MonStr Pair b -> Stream Bool
checkAux (MCons (Pair (a, as) (b, bs))) (MCons (Pair (a', as') (b', bs'))) = ((a == a') && (b == b')) <: (zipWithA (&&) (checkAux as as') (checkAux bs bs'))
           
x = pairStr 5
y = pairFuncStr 2

-- Careful: n > 18 starts getting very slow to calculate
checkInterchangeLaw n = takeS n (checkLaw (interchange x) x y)