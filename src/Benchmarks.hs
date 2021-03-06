module Benchmarks where

import Criterion
import Criterion.Main
import Criterion.Types
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.Native as LLVM
import qualified Data.Array.Accelerate.Interpreter as I

import Tree
import Utils
import AccTree

treeSizes :: [(Int, Int)]
treeSizes = zip (repeat 50) [30, 40, 50]

benchmarkConfig :: Config
benchmarkConfig = defaultConfig { timeLimit = 30.0, resamples = 1000 }

runAccelerateBenchmarks :: IO ()
runAccelerateBenchmarks = defaultMainWith benchmarkConfig (concat suite)
    where
        suite = map (
            \(w, h) -> [
                testAccConversion w h,
                testFindExpr w h,
                testGetParentCoords w h,
                testFindAncestors w h,
                testKey w h
            ]) treeSizes

runBenchmarks :: IO ()
runBenchmarks = defaultMainWith benchmarkConfig (concat benchSuite)
    where
        benchSuite =
            map (\(w, h) -> [testFindExprRec w h, testFindExprAcc w h]) treeSizes

testFindExprRec :: Int -> Int -> Benchmark
testFindExprRec w h = bench bName $ nf findAncestorsOfTypeRec tree
    where
        bName = "recursive: find nodes of type \'Expr\' (tree width " ++ show w ++ ", height " ++ show h ++ ")"
        tree = buildNLevelAST w h

testFindExprAcc :: Int -> Int -> Benchmark
testFindExprAcc w h = bench bName $ nf f accTree
    where
        bName = "accelerate: find nodes of type \'Expr\' (tree width " ++ show w ++ ", height " ++ show h ++ ")"
        f tree = LLVM.run
               $ findAncestorsOfType "Expr" tree

        accTree = astToNCTree (buildNLevelAST w h)

testAccConversion w h = bench bName $ nf f (buildNLevelAST w h)
    where
        bName = "accelerate: tree conversion"
        f tree = let (A.T3 nc _ _) = astToNCTree tree in I.run nc

testFindExpr w h = bench bName $ nf f nctree
    where
        bName = "accelerate: find nodes of type expr"
        nctree = astToNCTree (buildNLevelAST w h)
        f tree = LLVM.run $ findNodesOfType "Expr" tree

testGetParentCoords w h = bench bName $ nf f nc
    where
        bName = "accelerate: get parent coordinates"
        (A.T3 nc _ _) = astToNCTree (buildNLevelAST w h)
        f nc = LLVM.run $ getParentCoordinates nc

testFindAncestors w h = bench bName $ nf f tree
    where
        bName = "accelerate: find closest ancestors of type"
        tree = astToNCTree (buildNLevelAST w h)
        f t = LLVM.run $ findAncestorsOfType "Expr" t


testKey w h = bench bName $ nf f nc
    where
        bName = "accelerate: key operator"
        tree@(A.T3 nc _ _) = astToNCTree (buildNLevelAST w h)
        ancestors = findAncestorsOfType "Expr" tree
        f tree = LLVM.run (key2 (\a b -> b) ancestors tree) 

findAncestorsOfTypeRec :: AST -> [Char] -> [(ASTNode, ASTNode)]
findAncestorsOfTypeRec tree targetType =
    let root = treeRoot tree
    in findClosestAncestors tree root [(root, root)]
    where
        findClosestAncestors (Tree root []) _ res = res
        findClosestAncestors node@(Tree root children) currClosest res
            = mconcat [
                newRes,
                mconcat $ map (\node -> findClosestAncestors node newClosest []) children
            ]
            where
                newClosest = if nodeType root == targetType then root else currClosest
                newRes = res ++ zip (map treeRoot children) (repeat newClosest)
