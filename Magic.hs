module Magic (magic, condIncr, trim, enumerate) where

import           Prelude hiding (filter, fmap, foldl, foldr, liftA2, map, mapM,
                          mapM_, pure, replicate, return, reverse, sequence,
                          sequenceA, unzip, zip, zip3, zipWith, (*>), (<$),
                          (<$>), (<*), (<*>), (>>), (>>=))

{-

You are not allowed to use
    - list comprehensions
    - do-notation
    - any imports
    - any zips or unzips
    - any maps and their operator counterparts (including, but not limited to fmap, amap, mapM, mapM_)
    - any functions on lists and their operator counterparts (including but not limited to sorts, folds, filters, reverse, replicate)
    - (<$>), (<*>), (*>). (>>=), (>>).

Gradescope will check your submission for any of the prohibited syntax, functions, operators and imports.

 ** Basically, the rule is: if you want to use something, implement it yourself! **

Execution time and memory consumption are limited.

Memory consumption is limited to 4 GB.

Execution time is limited to 10 minutes for the whole submission. Some tests may have their own time limits. If a single timed test exceeds the limit, only that test will fail. If the whole submission takes longer than 10 minutes to run, you will receive 0 for the whole submission.

--}

{-
                                       ** THE TASK **

Implement the function

    magic :: (a -> (b -> c)) -> ([a] -> ([b] -> [c]))

so that you can define condIncr, trim, and enumerate with a *single* invocation of magic.

Your implementation must work correctly on any inputs, not just the ones in examples.

We have given these questions in ascending difficulty.


This task is worth 10 POINTS.

--}

-- Magic
--
magic :: (a -> (b -> c)) -> ([a] -> ([b] -> [c]))
magic f [] _ = []
magic f _ [] = []
magic f (x:xs) (y:ys) = (f x y) : (magic f xs ys)  
-- EASY
-- The function increments the number at index i in the first list if the second list contains True at the same index.
-- The result should have the same length as the shorter of the two lists.
--
-- Example:
-- condIncr [1, 2, 3, 4] [True, False, False, True] == [2, 2, 3, 5]
--
condIncr :: [Int] -> [Bool] -> [Int]

condIncr xs ys = magic f xs ys
    where
        f x y =  if y then x + 1 else x
--MEDIUM
-- The function takes two lists, and if the first list is longer than the second one, it trims it by dropping elements from the end until the lengths of the lists are equal.
--
-- Example:
-- trim [1, 2, 3] ['a', 'b', 'c', 'd'] == [1, 2, 3]
-- trim [1, 2, 3] ['a', 'b'] == [1, 2] 
--
trim :: [a] -> [b] -> [a]
trim xs ys = magic f xs ys
    where 
        f x y = x
-- HARD
-- A function to return the elements of a list with their indices, starting from 0.
--
-- Example:
-- enumerate ['a', 'b', 'c'] == [(0, 'a'), (1, 'b'), (2, 'c')] 
--
enumerate :: [a] -> [(Int, a)]
enumerate xs = magic f xs [0..]
    where
        f x y = (y, x)


