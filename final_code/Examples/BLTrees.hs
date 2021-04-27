{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Examples.BLTrees where
 
import Prelude hiding (head, tail, scanl, scanl1,
  iterate, take, drop, takeWhile,
  dropWhile, repeat, cycle, filter, (!!), 
  zip, unzip, zipWith, zip3, unzip3, zipWith3,
  words,unwords,lines,unlines, break, span, splitAt)
  
import MonadicStream
import Data.List (intercalate)
import Control.Applicative
import Data.Foldable

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
 | Some vommon operations on trees, translated into 
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