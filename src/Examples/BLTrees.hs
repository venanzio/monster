{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples.BLTrees where
 
import MonadicStreams hiding ((++))
import Data.List (intercalate, splitAt)
import Control.Applicative
import Control.Monad
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
 

-- | Functions to pretty print a branch-labelled tree
--
-- The trees are shown as unlabelled nodes and leaves,
-- with the branches between them represented as arrows
-- from parent nodes to their children.

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



-- * Some common operations on trees, translated into 'branch-labelled' versions


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


-- | Prunes a tree based on a predicate
pruneBy :: (a -> Bool) -> BLTree a -> BLTree a
pruneBy p (MCons blt) = MCons $ do (a, st) <- blt 
                                   guard (p a)
                                   return (a, pruneBy p st)


-- * BLT examples

-- * Generators for trees that always branch according 
-- to a given list

treeBranchList :: [a] -> BLTree a
treeBranchList l = node $ map (\a -> (a, treeBranchList l)) l

incrementingTree :: [Int] -> BLTree Int
incrementingTree l = node $ map (\a -> (a, incrementingTree (map (+1) l))) l


-- | Type to encode fractional numbers
type Fraction = (Integer, Integer)

-- | Reduces a labelled fraction to its simplest terms
reduceFrac :: LabelledNum Fraction -> LabelledNum Fraction
reduceFrac (LN (s, f)) = LN (s, reduce f)

reduce :: Fraction -> Fraction
reduce f@(a , b) = let gcf = gcd a b in if gcf == 0 then f else (a `div` gcf , b `div` gcf)

instance Num Fraction where
  (a, b) + (c, d) = if b == d then (a + c, d) else reduce ((a * d) + (b * c), b * d)
  (a, b) * (c, d) = reduce (a * c, b * d)
  abs (a, b) = (abs a, abs b)
  signum (a, b) = ((signum a) * (signum b), 1)
  fromInteger n = (n, 1)
  negate (a, b) = (-a, b)
  
data LabelledNum a = LN (String, a) deriving Eq

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
                  in (if nu == 1 then (ls ++ rs) else (ls ++ (reduceFrac (LN (s, (nu - 1, de))):rs)))

-- | Variation of (*) that throws away the first label
(*|) :: LabelledNum Fraction -> LabelledNum Fraction -> LabelledNum Fraction
(LN (s, n)) *| (LN (s', m)) = LN (s', n * m)


-- | Word tree example
wordTree = node [('a', node [(' ', node [('b', node [('a', node [('t', leaf)]),
                                                     ('u', node [('r', node [('e', node [('a', node [('u', node [('c', node [('r', node [('a', node [('t', leaf)])])])])])])])])
                                                    ]),
                                         ('c', node [('a', node [('t', leaf)])])
                                        ]),
                             ('n', node [(' ', node [('a', node [('p', node [('e', leaf)])])
                                                    ]),
                                         ('i', node [('m', node [('a', node [('l', leaf)])])])
                                        ])
                            ])
                ]

-- * Examples of basic branch-labelled trees

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
