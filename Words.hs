module Words (countWays) where


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

A word w is written on the blackboard. Dr. H. A. Skell noticed that by removing some letters from w, he can obtain a new word v. Now he is interested in how many ways there are to obtain the word v from the original word by removing some (possibly none) letters.

Implement the function

    countWays :: String -> String -> Integer

which will do the calculations for him. A word is any sequence (possibly empty) of lower-case English letters.

    Examples:

countWays "haskell" "hall" == 1
countWays "energybill" "eel" == 2

This problem is worth 10 POINTS.


        clearedString = clearString original target []
        clearString :: String -> String -> [Char] -> [Char]
        clearString [] target_s res = res
        clearString (c:cs) target_s res = clearString cs target_s (if inTarget c target_s then res ++ [c] else res)
        inTarget :: Char -> String -> Bool
        inTarget c [] = False
        inTarget c (c_t:c_ts) 
            | c == c_t = True
            | otherwise = inTarget c c_ts
--}

countWays :: String -> String -> Int
countWays original target = result
    where
        result = findNumWays original target 0

        findNumWays :: String -> String -> Int -> Int
        findNumWays original [] count = count
        findNumWays original (x:xs) count = findNumWays (second (removeOccurence original x [] 0)) xs (count + (first (removeOccurence original x [] 0)))
        
        removeOccurence :: String -> Char -> [Char] -> Int -> (Int, [Char])
        removeOccurence [] char res count = (count, res)
        removeOccurence (c:cs) char res count = removeOccurence cs char (if c == char then res else res ++ [c]) (if c == char then count + 1 else count)

        first:: (a, b) -> a
        first (x,y) = x

        second:: (a, b) -> b
        second (x,y) = y




