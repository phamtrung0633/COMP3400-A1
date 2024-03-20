module Words (countWays) where


import           Prelude hiding (filter, fmap, foldl, foldr, liftA2, map, mapM,
                          mapM_, pure, replicate, return, reverse, sequence,
                          sequenceA, unzip, zip, zip3, zipWith, (*>), (<$),
                          (<$>), (<*), (<*>), (>>), (>>=))

countWays :: String -> String -> Integer
countWays original [] = 1
countWays [] target = 0

countWays (o:os) (t:ts) 
    |   o == t    = (countWays os ts) + (countWays os (t:ts))
    |   otherwise = countWays os (t:ts)