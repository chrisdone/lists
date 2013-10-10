-- | Functions for dealing with lists.

module Data.Lists
  ( -- * Re-exports
   module Data.List
  ,module Data.List.Split
  ,module Data.List.Extras
  -- * List operations
  ,list
  ,unionOf
  ,for
  ,lastToMaybe
  ,firstOr
  ,maxList
  ,powerslice
  ,merge
  ,mergeBy
  ,hasAny
  ,takeWhileList
  ,dropWhileList
  ,spanList
  ,keysAL
  ,valuesAL
  ,breakList
  ,replace
  ,joins
  ,genericJoin
  ,addToAL
  ,delFromAL
  ,hasKeyAL
  ,flipAL
  ,strToAL
  ,strFromAL
  ,countElem
  ,elemRIndex
  ,alwaysElemRIndex
  ,seqList)
  where

import Data.List.Extras hiding (list)
import Data.Bools
import Data.List
import Data.Maybe
import Data.List.Split

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

{- | Merge two sorted lists into a single, sorted whole.

Example:

> merge [1,3,5] [1,2,4,6] -> [1,1,2,3,4,5,6]

QuickCheck test property:

prop_merge xs ys =
    merge (sort xs) (sort ys) == sort (xs ++ ys)
          where types = xs :: [Int]
-}
merge ::  (Ord a) => [a] -> [a] -> [a]
merge = mergeBy (compare)

{- | Merge two sorted lists using into a single, sorted whole,
allowing the programmer to specify the comparison function.

QuickCheck test property:

prop_mergeBy xs ys =
    mergeBy cmp (sortBy cmp xs) (sortBy cmp ys) == sortBy cmp (xs ++ ys)
          where types = xs :: [ (Int, Int) ]
                cmp (x1,_) (x2,_) = compare x1 x2
-}
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _cmp [] ys = ys
mergeBy _cmp xs [] = xs
mergeBy cmp (allx@(x:xs)) (ally@(y:ys))
        -- Ordering derives Eq, Ord, so the comparison below is valid.
        -- Explanation left as an exercise for the reader.
        -- Someone please put this code out of its misery.
    | (x `cmp` y) <= EQ = x : mergeBy cmp xs ally
    | otherwise = y : mergeBy cmp allx ys

{- | Returns true if the given list contains any of the elements in the search
list. -}
hasAny :: Eq a => [a]           -- ^ List of elements to look for
       -> [a]                   -- ^ List to search
       -> Bool                  -- ^ Result
hasAny [] _ = False             -- An empty search list: always false
hasAny _ [] = False             -- An empty list to scan: always false
hasAny search (x:xs) = if x `elem` search then True else hasAny search xs

{- | Similar to Data.List.takeWhile, takes elements while the func is true.
The function is given the remainder of the list to examine. -}
takeWhileList :: ([a] -> Bool) -> [a] -> [a]
takeWhileList _ [] = []
takeWhileList func list'@(x:xs) =
    if func list'
       then x : takeWhileList func xs
       else []

{- | Similar to Data.List.dropWhile, drops elements while the func is true.
The function is given the remainder of the list to examine. -}
dropWhileList :: ([a] -> Bool) -> [a] -> [a]
dropWhileList _ [] = []
dropWhileList func list'@(_:xs) =
    if func list'
       then dropWhileList func xs
       else list'

{- | Similar to Data.List.span, but performs the test on the entire remaining
list instead of just one element.

@spanList p xs@ is the same as @(takeWhileList p xs, dropWhileList p xs)@
-}
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])

spanList _ [] = ([],[])
spanList func list'@(x:xs) =
    if func list'
       then (x:ys,zs)
       else ([],list')
    where (ys,zs) = spanList func xs

{- | Similar to Data.List.break, but performs the test on the entire remaining
list instead of just one element.
-}
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

{- | Given a list and a replacement list, replaces each occurance of the search
list with the replacement list in the operation list.

Example:

>replace "," "." "127,0,0,1" -> "127.0.0.1"

This could logically be thought of as:

>replace old new l = join new . split old $ l
-}

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = joins new . splitOn old $ l

{- | Given a delimiter and a list of items (or strings), join the items
by using the delimiter.

Example:

> join "|" ["foo", "bar", "baz"] -> "foo|bar|baz"
-}
joins :: [a] -> [[a]] -> [a]
joins delim l = concat (intersperse delim l)

{- | Like 'join', but works with a list of anything showable, converting
it to a String.

Examples:

> genericJoin ", " [1, 2, 3, 4] -> "1, 2, 3, 4"
> genericJoin "|" ["foo", "bar", "baz"] -> "\"foo\"|\"bar\"|\"baz\""

-}
genericJoin :: Show a => String -> [a] -> String
genericJoin delim l = joins delim (map show l)

{- | Adds the specified (key, value) pair to the given list, removing any
existing pair with the same key already present. -}
addToAL :: Eq key => [(key, elt)] -> key -> elt -> [(key, elt)]
addToAL l key value = (key, value) : delFromAL l key

{- | Removes all (key, value) pairs from the given list where the key
matches the given one. -}
delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = filter (\a -> (fst a) /= key) l

{- | Returns the keys that comprise the (key, value) pairs of the given AL.

Same as:

>map fst
-}
keysAL :: [(key, a)] -> [key]
keysAL = map fst

{- | Returns the values the comprise the (key, value) pairs of the given
AL.

Same as:

>map snd
-}
valuesAL :: [(a, value)] -> [value]
valuesAL = map snd

{- | Indicates whether or not the given key is in the AL. -}
hasKeyAL :: Eq a => a -> [(a, b)] -> Bool
hasKeyAL key list' =
    elem key (keysAL list')

{- | Flips an association list.  Converts (key1, val), (key2, val) pairs
to (val, [key1, key2]). -}
flipAL :: (Eq key, Eq val) => [(key, val)] -> [(val, [key])]
flipAL oldl =
    let worker :: (Eq key, Eq val) => [(key, val)] -> [(val, [key])] -> [(val, [key])]
        worker [] accum = accum
        worker ((k, v):xs) accum =
            case lookup v accum of
                                Nothing -> worker xs ((v, [k]) : accum)
                                Just y -> worker xs (addToAL accum v (k:y))
        in
        worker oldl []

{- | Converts an association list to a string.  The string will have
one pair per line, with the key and value both represented as a Haskell string.

This function is designed to work with [(String, String)] association lists,
but may work with other types as well. -}

strFromAL :: (Show a, Show b) => [(a, b)] -> String
strFromAL inp =
    let worker (key, val) = show key ++ "," ++ show val
        in unlines . map worker $ inp

{- | The inverse of 'strFromAL', this function reads a string and outputs the
appropriate association list.

Like 'strFromAL', this is designed to work with [(String, String)] association
lists but may also work with other objects with simple representations.
-}
strToAL :: (Read a, Read b) => String -> [(a, b)]
strToAL inp =
    let worker line =
            case reads line of
               [(key, remainder)] -> case remainder of
                     ',':valstr -> (key, read valstr)
                     _ -> error "Data.List.Utils.strToAL: Parse error on value"
               _ -> error "Data.List.Utils.strToAL: Parse error on key"
        in map worker (lines inp)


{- FIXME TODO: sub -}

{- | Returns a count of the number of times the given element occured in the
given list. -}
countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)

{- | Returns the rightmost index of the given element in the
given list. -}
elemRIndex :: Eq a => a -> [a] -> Maybe Int
elemRIndex item l =
    case reverse $ elemIndices item l of
      [] -> Nothing
      (x:_) -> Just x

{- | Like elemRIndex, but returns -1 if there is nothing
found. -}
alwaysElemRIndex :: Eq a => a -> [a] -> Int
alwaysElemRIndex item list' =
    case elemRIndex item list' of
       Nothing -> -1
       Just x -> x

{- | Forces the evaluation of the entire list. -}
seqList :: [a] -> [a]
seqList [] = []
seqList list'@(_:xs) = seq (seqList xs) list'
