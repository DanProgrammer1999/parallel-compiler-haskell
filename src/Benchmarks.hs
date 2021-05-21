module Benchmarks where

import Criterion
import Criterion.Main
import Criterion.Types
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as LLVM
import qualified Data.Array.Accelerate.Interpreter as I

import Tree
import Demo
import AccTree

treeSizes :: [(Int, Int)]
treeSizes = zip (repeat 6) [3, 4, 5]

benchmarkConfig :: Config
benchmarkConfig = defaultConfig { timeLimit = 15.0, resamples = 1000 }

runBenchmarks :: IO ()
runBenchmarks = defaultMainWith benchmarkConfig (concat benchSuite)
    where
        benchSuite =
            map (\(w, h) -> [testFindExprRec w h, testFindExprAcc w h]) treeSizes

testFindExprRec :: Int -> Int -> Benchmark
testFindExprRec w h = bench bName $ nf (foldr f 0) (buildNLevelAST w h)
    where
        bName = "recursive: find nodes of type \'Expr\' (tree width " ++ show w ++ ", height " ++ show h ++ ")"
        f :: ASTNode -> Int -> Int
        f (ASTNode t v) c = if t == "Expr" then c + 1 else c

testFindExprAcc :: Int -> Int -> Benchmark
testFindExprAcc w h = bench bName $ nf f accTree
    where
        bName = "accelerate: find nodes of type \'Expr\' (tree width " ++ show w ++ ", height " ++ show h ++ ")"
        f tree = A.arraySize
               $ LLVM.run
               $ findNodesOfType "Expr" tree

        accTree = astToNCTree (buildNLevelAST w h)

-- findClosestAncestorsOfTypeRec :: AST -> String -> [(ASTNode, ASTNode)]
findClosestAncestorsOfTypeRec tree nodeType = addId tree 0 0
    where
        addId node@(Tree root children) currId level =
            Tree (currId, root) (zipWith (\node id -> addId node id (length children)) children (map (* level) [currId + 1..]))

f tree = A.arraySize
    $ LLVM.run
    $ findNodesOfType "Expr" (astToNCTree tree)
