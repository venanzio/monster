module Examples.LazyLists where

import Prelude hiding (head, tail, map, scanl, scanl1,
  iterate, take, drop, takeWhile, (++),
  dropWhile, repeat, cycle, filter, (!!), 
  zip, unzip, zipWith, zip3, unzip3, zipWith3,
  words,lines,unlines, break, span, splitAt)
  
import MonadicStream
import Data.Foldable

type LList a = MonStr Maybe a

nil :: LList a
nil = MCons Nothing

cons :: a -> LList a -> LList a
cons a l = MCons (Just (a, l))

-- | Conversion between native and monster representation of
-- lazy lists
llist :: [a] -> LList a
llist = foldr cons nil

-- | Uses the Foldable instance of monadic streams, the
-- inverse of llist
fromL :: LList a -> [a]
fromL = toList


-- | Example lists
natsLess10 :: LList Int
natsLess10 = llist [0..9]

natsS :: LList Int
natsS = llist [0..]

sentence :: LList Char
sentence = unwordsMFL (llist ["we","want","a","shrubbery"])