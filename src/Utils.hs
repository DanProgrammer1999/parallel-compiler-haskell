module Utils where

padRight :: Int -> a -> [a] -> [a]
padRight n e arr 
    | length arr == n = arr
    | otherwise       = arr ++ replicate (n - length arr) e

padToEqualLength :: a -> [[a]] -> [[a]]
padToEqualLength e arr = map (padRight maxLength e) arr
    where
        maxLength = maximum $ map length arr
