module FirstCommon (firstCommon) where


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



--                                       ** THE TASK **

-- Implement a function which will find the most common element in a list. 
-- If there are several elements with this property, return the one that is encountered earlier. 
-- We guarantee that the list will contain at least one element.
--
--  Examples:
--
-- fistCommon [1, 2, 3, 1] == 1
-- firstCommon [1] == 1
-- firstCommon [2, 3, 1] == 2
--
-- This task is worth 10 POINTS.


firstCommon :: Eq a => [a] -> a

firstCommon listItems = result 
    where
        result = firstItemWithFreq freqList listItems maxFreq
        firstItemWithFreq :: [Integer] -> [a] -> Integer -> a
        firstItemWithFreq (x:xs) (y:ys) target_freq 
            | x == target_freq = y
            | otherwise = firstItemWithFreq xs ys target_freq

        maxFreq = maxFrequency freqList 0 
        maxFrequency :: [Integer] -> Integer -> Integer
        maxFrequency [] res = res
        maxFrequency (x:xs) res = maxFrequency xs (if x > res then x else res)

        freqList = frequencyList listItems listItems []
        frequencyList :: Eq a => [a] -> [a] -> [Integer] -> [Integer]
        frequencyList [] original result = result
        frequencyList (x:xs) original result = frequencyList xs original (result ++ [(getFrequency x original 0)]) 

        getFrequency :: Eq a => a -> [a] -> Integer -> Integer
        getFrequency a [] res = res
        getFrequency a (x:xs) res = getFrequency a xs (if x == a then res + 1 else res)




