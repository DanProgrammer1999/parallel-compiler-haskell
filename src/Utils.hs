module Utils where

import Tree

padRight :: Int -> a -> [a] -> [a]
padRight n e arr 
    | length arr == n = arr
    | otherwise       = arr ++ replicate (n - length arr) e

padToEqualLength :: a -> [[a]] -> [[a]]
padToEqualLength e arr = map (padRight maxLength e) arr
    where
        maxLength = maximum $ map length arr

buildNLevelAST :: Int -> Int -> AST
buildNLevelAST width n
    | n <= 0 = astLeafNode "Num" "42"
    | otherwise =
        Tree (ASTNode "Expr" "")
        [
            Tree (ASTNode "Op" "+")
            (replicate width (buildNLevelAST width (n - 2)))
        ]