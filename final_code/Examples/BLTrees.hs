{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples.BLTrees where
 
import MonadicStreams hiding ((++))
import Data.List (intercalate, splitAt)
import Control.Applicative
import Data.Foldable
import Data.Bifunctor

type BLTree a = MonStr [] a

-- | Contructors, which join branches together
-- by acting as 'anonymous' nodes and leaves

leaf :: BLTree a
leaf = MCons []

node :: [(a, BLTree a)] -> BLTree a
node l = MCons l

-- | Joins a list of BLTs together at their root node
branch :: [BLTree a] -> BLTree a
branch = foldl (<|>) empty
 

{-
 | Functions to pretty print a branch-labelled tree

 The trees are shown as unlabelled nodes and leaves,
 with the branches between them represented as arrows
 from parent nodes to their children.
-}

showTree :: Show a => BLTree a -> String
showTree = intercalate "\n" . stList
    
instance Show a => Show (BLTree a) where
  show = showTree

strListPrefixes :: String -> String -> [String] -> [String]
strListPrefixes s0 snext [] = []
strListPrefixes s0 snext (s:ls) = (s0++s) : map (snext++) ls

stPair :: Show a => String -> (a, BLTree a) -> [String]
stPair pre (a,t) = let prefix = pre ++ "- "++ show a ++ " -> "
                       blanks = replicate (length prefix) ' ' 
                   in strListPrefixes prefix blanks (stList t)

stPairList :: Show a => [(a, BLTree a)] -> [String]
stPairList [] = ["leaf"]
stPairList (p:ps) = stPair "node [ " p ++ concat (map (stPair "     , ") ps) ++ ["     ]"]

stList :: Show a => BLTree a -> [String]
stList = stPairList . uncons

printTree :: Show a => BLTree a -> IO ()
printTree = putStrLn . showTree


{-
 | Some common operations on trees, translated into 
 'branch-labelled tree' versions
-}

-- | Depth-first traversal
dfLabels :: BLTree a -> [a]
dfLabels (MCons l) =  concat (map (\(a,l') -> a:dfLabels l') l)

-- | Breath-first traversal, the same as toList from Foldable
bfLabels :: BLTree a -> [a]
bfLabels t = bfLabs [t]
             where 
                bfLabs :: [BLTree a] -> [a]
                bfLabs [] = []
                bfLabs ((MCons l):ts) = map fst l ++ bfLabs (ts ++ map snd l)

-- | Lets you make a choice of which subtree to traverse
subtree :: Int -> BLTree a -> BLTree a
subtree n (MCons []) = error "Examples.BLTrees.subtree: subtree index out of bounds"
subtree 0 (MCons ((_, t):ts)) = t
subtree n (MCons ((_, t):ts)) = subtree (n - 1) (MCons ts)

-- | Bottom-leftmost element in a tree
leftmost :: BLTree a -> a
leftmost (MCons []) = error "Empty tree"
leftmost (MCons ((b, MCons []):bs)) = b
leftmost (MCons ((b, ts):bs)) = leftmost ts
        
-- | Bottom-rightmost element in a tree      
rightmost :: BLTree a -> a
rightmost (MCons []) = error "Empty tree"
rightmost (MCons ((b, MCons []):[])) = b
rightmost (MCons ((b, ts):[])) = rightmost ts
rightmost (MCons ((b, ts):bs)) = rightmost (MCons bs)


-- | BLT examples

-- | Type to encode fractional numbers
type Fraction = (Integer, Integer)

instance Num Fraction where
  (a, b) + (c, d) = (a + c, d)
  (a, b) * (c, d) = (a * c, b * d)
  abs (a, b) = (abs a, abs b)
  signum (a, b) = ((signum a) * (signum b), 1)
  fromInteger n = (n, 1)
  negate (a, b) = (-a, b)
  
data LabelledNum a = LN (String, a)

instance Num a => Num (LabelledNum a) where
  (LN (s, n)) + (LN (s', m)) = LN (s ++ " | " ++ s', n + m)
  (LN (s, n)) * (LN (s', m)) = LN (s ++ " & " ++ s', n * m)
  abs (LN (s, n)) = LN (s, abs n)
  signum (LN (s, n)) = LN (s, signum n)
  fromInteger n = LN ("", fromInteger n)
  negate (LN (s, n)) = LN (s, -n)
  
instance Show a => Show (LabelledNum a) where
  show (LN (s,a)) = s ++ ": " ++ show a

instance Functor LabelledNum where
   fmap f (LN (s, a)) = LN (s, f a)
   
-- | Tree representing probability tree of taking one of n
-- balls from a bag, where there are m different colours,
-- with an amount a_0, a_1, ... , a_m of each colour. 
-- 
-- Each level in the tree is a new ball taken, with 
-- probabilites modified based on the previous one
-- taken.
genCondProbTree :: [LabelledNum Fraction] -> BLTree (LabelledNum Fraction)
genCondProbTree ps = MCons $ map (\(p, t) -> (p, genCondProbTree t)) (zip ps [pickOne n ps | n <- [0..(length ps - 1)]])

pickOne :: Int -> [LabelledNum Fraction] -> [LabelledNum Fraction]
pickOne n ps = let (ls, (LN (s, (nu, de))):rs) = splitAt n (map (fmap (bimap id (\x -> x - 1))) ps) 
                  in (if nu == 1 then (ls ++ rs) else (ls ++ ((LN (s, (nu - 1, de))):rs)))

-- | Variation of (*) that throws away the first label
(*|) :: LabelledNum Fraction -> LabelledNum Fraction -> LabelledNum Fraction
(LN (s, n)) *| (LN (s', m)) = LN (s', n * m)


-- | Examples of basic branch-labelled trees

blt1 = node [(5, leaf),
             (9, node [(1, leaf)]),
             (2, node [(4, node [(3,leaf),(6,leaf)]),
                       (7,leaf)])
            ]

blt2 = node [(10, node [(11,leaf)
                       ,(12,node [(13,leaf)])
                       ]
             )
            ,(20, leaf)
            ]

{-
                    _.---._
                _.-~       ~-._
            _.-~               ~-._
        _.-~                       ~---._
    _.-~                                 ~\
 .-~               BLT                  _.;
 :-._                               _.-~ ./
 `-._~-._                   _..__.-~ _.-~
  /  ~-._~-._              / .__..--~----._
 \_____(_;-._\.        _.-~_/       ~).. . \
    /(_____  \`--...--~_.-~______..-+_______)
  .(_________/`--...--~/    _/nad        /\
 /-._     \_     (___./_..-~__.....__..-~./
 `-._~-._   ~\--------~  .-~_..__.-~ _.-~
     ~-._~-._ ~---------'  / .__..--~
         ~-._\.        _.-~_/
             \`--...--~_.-~
              `--...--~ 
           
http://www.ascii-art.de/ascii/s/sandwich.txt
-}