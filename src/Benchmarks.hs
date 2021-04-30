module Benchmarks where

import Criterion
import Criterion.Main
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as A

import Tree
import Demo
import AccTree

runBenchmarks :: IO ()
runBenchmarks = defaultMain benchSuite
    where
        benchSuite
            = foldr
            (\n arr -> testFindExprAcc n : testFindExprRec n : arr)
            []
            [30, 40, 50]

testFindExprRec :: Int -> Benchmark
testFindExprRec n = bench bName $ nf (foldr f 0) (buildNLevelAST n)
    where
        bName = "recursive: find nodes of type \'Expr\' (tree size " ++ show n ++ ")"
        f :: ASTNode -> Int -> Int
        f (ASTNode t v) c = if t == "Expr" then c + 1 else c

testFindExprAcc :: Int -> Benchmark
testFindExprAcc n = bench bName $ nf f (buildNLevelAST n)
    where
        bName = "accelerate: find nodes of type \'Expr\' (tree size " ++ show n ++ ")"
        f :: AST -> Int
        f tree = length
               $ A.toList
               $ A.run
               $ findNodesOfType "Expr" (astToNCTree tree)