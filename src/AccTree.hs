{-# LANGUAGE TypeOperators, ViewPatterns, FlexibleContexts, RebindableSyntax, TypeFamilies #-}

module AccTree where

import Data.Array.Accelerate     as A
import qualified Prelude         as P
import Utils
import Tree

type VectorTree = (Vector Int, Matrix Char, Matrix Char)
type NCTree = Acc (Matrix Int, Matrix Char, Matrix Char)

treeToVectorTree :: AST -> VectorTree
treeToVectorTree tree = (depthAcc, typesAcc, valuesAcc)
    where
        treeToList currLevel (Tree root children)
            = (currLevel, nodeType root, nodeValue root)
            : P.concatMap (treeToList (currLevel + 1)) children

        listTree = treeToList 0 tree

        depthVector = P.map (\(d, _, _) -> d) listTree
        types  = padToEqualLength '\0' $ P.map (\(_, t, _) -> t) listTree
        values = padToEqualLength '\0' $ P.map (\(_, _, v) -> v) listTree

        maxLength arr = P.maximum (P.map P.length arr)

        depthAcc = fromList (Z :. P.length depthVector) depthVector
        typesAcc = fromList (Z :. P.length types :. maxLength types) (P.concat types)
        valuesAcc = fromList (Z :. P.length values :. maxLength values) (P.concat values)        

constructNodeCoordinates :: Acc (Vector Int) -> Acc (Matrix Int)
constructNodeCoordinates depthVec = nodeCoordinates
    where
        maxDepth = the $ maximum depthVec
        nodeCount = size depthVec

        depthMatrix
            = generate
            (I2 nodeCount (maxDepth + 1))
            (\(I2 i j) -> boolToInt (depthVec !! i == j))

        cumulativeMatrix = transpose $ scanl1 (+) $ transpose depthMatrix

        dropExtraNumbers (I2 i j) e = boolToInt (j <= depthVec !! i) * e
        nodeCoordinates = imap dropExtraNumbers cumulativeMatrix

vectorToNCTree :: VectorTree -> NCTree
vectorToNCTree (depthVec, types, values) =
    let
        depthVec' = use depthVec
        types' = use types
        values' = use values
        nodeCoordinates = constructNodeCoordinates depthVec'
    in lift (nodeCoordinates, types', values')

astToNCTree :: AST -> NCTree
astToNCTree = vectorToNCTree . treeToVectorTree

findNodesOfType :: [Char] -> NCTree -> Acc (Matrix Int)
findNodesOfType query (T3 nc types _)
    = reshape (I2 (size resVector `div` maxDepth) maxDepth) resVector
    where
        (I2 nRows typeLength) = shape types
        (I2 _ maxDepth) = shape nc 

        queryAcc = use (fromList (Z :. P.length query) query)
        compareSymbols sh@(I2 _ j) = 
            let val = types ! sh 
            in (j >= lift (P.length query) && val == lift '\0')
                || val == queryAcc ! I1 j

        boolMatrix = replicate (lift (Z :. All :. maxDepth)) $ and $ generate (shape types) compareSymbols
        resVector = afst $ compact boolMatrix nc     

getParentCoordinates :: Acc (Matrix Int) -> Acc (Matrix Int)
getParentCoordinates nc = generate (I2 nodeCount depth) genElement
    where
        (I2 nodeCount depth) = shape nc
        genElement (I2 i j) =
            if j + 1 == depth || nc ! I2 i (j + 1) == 0
            then 0
            else nc ! I2 i j

findAncestorsOfType :: [Char] -> NCTree -> Acc (Array DIM2 Int)
findAncestorsOfType query tree@(T3 nc _ _)
    = backpermute (shape nc) (closestAncestorMat !) focusNodes
    where
        focusNodes = findNodesOfType query tree
        parentCoords = getParentCoordinates nc
        (I2 focusNodesCount maxDepth) = shape parentCoords
        (I2 nodeCount _) = shape nc

        focusNodesExt = replicate (lift (Z :. nodeCount :. All :. All)) focusNodes
        parentCoordsExt = replicate (lift (Z :. All :. focusNodesCount :. All)) parentCoords

        isAncestorMatrix
            = map boolToInt
            $ fold1 (&&)
            $ zipWith (\a b -> a == b || b == 0) parentCoordsExt focusNodesExt

        closestAncestorVec
            = fold1 max
            $ imap (\(I2 i j) e -> e*j) isAncestorMatrix

        closestAncestorMat
            = imap (\(I2 i j) e -> lift (Z :. e :. j))
            $ replicate (lift (Z :. All :. maxDepth)) closestAncestorVec

-- Inner product of 2 matrices (general case of matrix multiplication with custom product and sum combinators)
innerProduct
    :: (Elt a, Elt b, Elt c)
    => (Exp a -> Exp b -> Exp c)  -- product function: how to combine 2 elements from two matrices
    -> (Exp c -> Exp c -> Exp c)  -- sum function: how to combine the row of results into single element
    -> Acc (Matrix a)             -- ma x x
    -> Acc (Matrix b)             -- x x nb 
    -> Acc (Matrix c)
innerProduct prodF sumF a b = fold1 sumF $ zipWith prodF aExt bExt
    where
        -- na == nb - precondition
        (I2 ma _) = shape a
        (I2 nb _) = shape b

        aExt = replicate (lift (Z :. All :. nb :. All)) a
        bExt = replicate (lift (Z :. ma :. All :. All)) b

uniqueRows :: (Eq a, Elt a) => Acc (Matrix a) -> Acc (Matrix a)
uniqueRows arr = uniqueKeys
    where
        identicalKeys = imap (\(I2 _ j) v -> boolToInt v * (j + 1)) $ innerProduct (==) (&&) arr arr
        identityVec = map (subtract 1) $ fold1 (\a b -> if a == 0 || b == 0 then a + b else min a b) identicalKeys

        uniqueMask = zipWith (==) (enumFromN (shape identityVec) 0) identityVec
        (T2 uniqueIdx _) = compact uniqueMask identityVec
        (I1 n) = shape uniqueIdx
        (I2 _ m) = shape arr
        uniqueKeys = backpermute (I2 n m) (\(I2 i j) -> I2 (uniqueIdx ! I1 i) j) arr

key :: (Shape sh, Shape sh', Elt k, Elt v, Elt r)
    => (Acc (Array sh k) -> Acc (Array (sh :. Int) v) -> Acc (Array sh' r))
    -> Acc (Array (sh :. Int) k)
    -> Acc (Array (sh :. Int) v)
    -> Acc (Array (sh :. Int) k, Array (sh' :. Int) r)
key f keys vals = undefined