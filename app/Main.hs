module Main where

main :: IO ()
main = putStrLn "Test"

testKeys = ["1110000", "1000000"]
testTree = ["1000000", "1110000", "1100000", "1111000", "1111100", "1111110"]

selectChildren :: String -> [String] -> [String]
selectChildren ancestor = filter (\child -> and(zipWith compareSymbols child ancestor))
    where
        compareSymbols c1 c2 = c1 == c2 || c2 == '0'
