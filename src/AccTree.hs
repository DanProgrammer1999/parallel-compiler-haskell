{-# LANGUAGE TypeOperators, ViewPatterns, FlexibleContexts, RebindableSyntax, TypeFamilies #-}

module AccTree where

import Data.Array.Accelerate             as A
import Data.Array.Accelerate.Data.Monoid as A
import qualified Prelude                 as P
import Utils
import Tree

type VectorTree = (Vector Int, Matrix Char, Matrix Char)
type NCTree = Acc (Matrix Int, Matrix Char, Matrix Char)

-- Tree to accelerate

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

-- Main logic

findNodesOfType :: [Char] -> NCTree -> Acc (Matrix Int)
findNodesOfType query (T3 nc types _)
    = selectRows selector nc
    where
        (I2 _ maxDepth) = shape nc
        queryAcc = use (fromList (Z :. P.length query) query)
        
        -- either index is out of bounds of query, and the element is zero, or elements match
        isValidCharacter (I2 _ j) c =
            (j >= size queryAcc && c == constant '\0') || c == queryAcc ! I1 j
        selector = afst
                 $ filter (>= 0)
                 $ imap (\(I1 i) v -> boolToInt v * (i + 1) - 1)
                 $ and (imap isValidCharacter types)

getParentCoordinates :: Acc (Matrix Int) -> Acc (Matrix Int)
getParentCoordinates nc = imap (\i e -> boolToInt (not (isReplacedWithZero i)) * e) nc
    where
        (I2 _ nCols) = shape nc
        isReplacedWithZero (I2 i j) = j + 1 == nCols || nc ! I2 i j == 0

findAncestorsOfType :: [Char] -> NCTree -> Acc (Array DIM2 Int)
findAncestorsOfType query tree@(T3 nc _ _)
    = selectRows closestAncestorIndexVec focusNodes
    where
        focusNodes = findNodesOfType query tree
        parentCoords = getParentCoordinates nc
        (I2 focusNodesCount maxDepth) = shape parentCoords

        closestAncestorIndexVec = fold1 max $ imap
            (\(I2 i j) e -> boolToInt e * j)
            (innerProduct (\a b -> a == b || b == 0) (&&) parentCoords focusNodes)

-- 2-dimensional version of key operator
-- First parameter is not used for now
key2 :: (Elt k, Elt v, Elt r, Eq k)
    => (Acc (Vector k) -> Acc (Vector v) -> Acc (Vector r))
    -> Acc (Matrix k)
    -> Acc (Matrix v)
    -> Acc (Matrix v, Segments Int)
key2 _ keys vals = T2 (selectRows selectors' vals) descriptor
    where
        (I2 nKeysRows nKeysCols) = shape keys

        -- each row contains indexes of equal rows
        -- groupsMatrix: nKeysRows x nKeysRows
        groupsMatrix = imap (\(I2 _ j) v -> boolToInt v * (j + 1) - 1) $ innerProduct (==) (&&) keys keys

        -- if a or b is zero, min a b is zero, and if not, (a == 0 || b == 0) is zero
        chooseMinId a b = if a < 0 || b < 0 then max a b else min a b
        uniqueRowsIdxVec = fold1 chooseMinId groupsMatrix
        uniqueMaskMat =
            replicate (lift (Z :. All :. nKeysRows))
            $ zipWith (==) (enumFromN (shape uniqueRowsIdxVec) 0) uniqueRowsIdxVec

        selectors = afst $ compact uniqueMaskMat groupsMatrix
        correctShape = I2 (unindex1 (shape selectors) `div` nKeysRows) nKeysRows
        (T2 selectors' descriptor) = filter (>= 0) (reshape correctShape selectors)
        
-- general version of the key operator
key :: (Shape sh, Shape sh', Elt k, Elt v, Elt r)
    => (Acc (Array sh k) -> Acc (Array sh' v) -> Acc (Array sh' r))
    -> Acc (Array (sh :. Int) k)
    -> Acc (Array (sh' :. Int) v)
    -> Acc (Array (sh' :. Int) r, Vector Int)
key f keys vals = undefined

-- General functions

-- Inner product of 2 matrices (general case of matrix multiplication with custom product and sum combinators)
innerProduct :: (Elt a, Elt b, Elt c)
    => (Exp a -> Exp b -> Exp c)  -- product function: how to combine 2 elements from two matrices
    -> (Exp c -> Exp c -> Exp c)  -- sum function: how to combine the row of results into single element
    -> Acc (Matrix a)             -- ma x x
    -> Acc (Matrix b)             -- x x nb 
    -> Acc (Matrix c)
innerProduct prodF sumF a b = fold1 sumF $ zipWith prodF aExt bExt
    where
        -- r1 == c2 - precondition
        (I2 r1 _) = shape a
        (I2 _ c2) = shape b

        aExt = replicate (lift (Z :. All :. c2 :. All)) a
        bExt = replicate (lift (Z :. r1 :. All :. All)) b

selectRows :: (Elt a) => Acc (Vector Int) -> Acc (Matrix a) -> Acc (Matrix a)
selectRows rowIndex arr = zipWith (\i j -> arr ! I2 i j) rowIndexMat colIndexMat
    where
        nResultRows = size rowIndex
        (I2 _ nCols) = shape arr
        rowIndexMat = replicate (lift (Z :. All :. nCols)) rowIndex
        colIndexMat = replicate (lift (Z :. nResultRows :. All))
            $ enumFromN (I1 nCols) (0 :: Exp Int)