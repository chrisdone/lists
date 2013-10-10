-- | Extra functions for dealing with lists.

module Data.Lists
  (list
  ,unionOf
  ,for
  ,lastToMaybe
  ,firstOr
  ,maxList
  ,powerslice
  ,module Data.List)
  where

import Data.Bools
import Data.List
import Data.Maybe

-- | When a list is non-null, pass it to a function, otherwise use the
-- default.
list :: b -> ([a] -> b) -> [a] -> b
list nil cons = cond (const nil) cons null

-- | Get the union of the given lists.
unionOf :: (Eq a) => [[a]] -> [a]
unionOf = foldr union []

-- | Opposite of map.
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Maybe get the last element in the list.
lastToMaybe :: [a] -> Maybe a
lastToMaybe [x]    = Just x
lastToMaybe (_:xs) = lastToMaybe xs
lastToMaybe []     = Nothing

-- | Return the first item of a list or something else.
firstOr :: a -> [a] -> a
firstOr n = fromMaybe n . listToMaybe

-- | Get the maximum of a list or return zero.
maxList :: (Num t, Ord t) => [t] -> t
maxList [] = 0
maxList xs = maximum xs

-- | Essentially a powerset but retaining contiguously ordererd subsets.
powerslice :: [a] -> [[a]]
powerslice xs = [] : concatMap (tail . inits) (tails xs)
