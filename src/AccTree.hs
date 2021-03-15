{-# LANGUAGE TypeOperators, ViewPatterns, FlexibleContexts, RebindableSyntax #-}

module AccTree where

import Data.Array.Accelerate     as A
import qualified Prelude         as P
import Data.Array.Accelerate.Interpreter (run)

import Utils

data Tree a = Tree 
    { treeRoot     :: a
    , treeChildren :: [Tree a]
    }

data ASTNode = ASTNode 
    { nodeType  :: P.String
    , nodeValue :: P.String
    } deriving (P.Show)

type AST = Tree ASTNode
type VectorTree = (Vector Int, Matrix Char, Matrix Char)
type NCTree = (Acc (Matrix Int), Matrix Char, Matrix Char)

vectoriseTree :: AST -> [(P.Int, P.String, P.String)]
vectoriseTree = vectoriseTree' 0
    where
        vectoriseTree' currLevel (Tree root children)  
            = (currLevel, nodeType root, nodeValue root)
            : P.concatMap (vectoriseTree' (currLevel + 1)) children

treeToAccelerate :: [(P.Int, P.String, P.String)] -> VectorTree
treeToAccelerate vTree = (depthAcc, typesAcc, valuesAcc)
    where 
        depthVector = P.map (\(d, _, _) -> d) vTree
        types  = padToEqualLength '\0' $ P.map (\(_, t, _) -> t) vTree
        values = padToEqualLength '\0' $ P.map (\(_, _, v) -> v) vTree

        depthAcc = fromList (Z :. P.length depthVector) depthVector

        maxLength arr = P.maximum (P.map P.length arr)
        
        typesAcc = fromList (Z :. P.length types :. maxLength types) (P.concat types)
        valuesAcc = fromList (Z :. P.length values :. maxLength values) (P.concat values)

constructNodeCoordinates :: Acc (Vector Int) -> Acc (Matrix Int)
constructNodeCoordinates depthVec = nodeCoordinates 
    where 
        maxDepth = the $ maximum depthVec
        nodeCount = size depthVec

        -- given an index, generate a single row of depth matrix
        generateDMRow (I2 i j) = if depthVec !! i == j then 1 else 0
        depthMatrix = generate (I2 nodeCount (maxDepth + 1)) generateDMRow
        cumulativeMatrix = transpose $ scanl1 (+) $ transpose depthMatrix

        dropExtraNumbers (I2 i j) e = if j > depthVec !! i then 0 else e
        nodeCoordinates = imap dropExtraNumbers cumulativeMatrix

vectorToNCTree :: VectorTree -> NCTree
vectorToNCTree (depthVec, types, values) =
    let 
        depthVec' = use depthVec
        types' = use types
        values' = use values
        nodeCoordinates = constructNodeCoordinates depthVec'
    in (nodeCoordinates, types, values)

astToNCTree :: AST -> (Acc (Matrix Int), Matrix Char, Matrix Char)
astToNCTree = vectorToNCTree . treeToAccelerate . vectoriseTree

findNodesOfType :: [Char] -> NCTree -> Acc (Matrix Int)
findNodesOfType query (nc, types, _) = reshape correctShape resVector
    where
        (Z :. _ :. typeLength) = arrayShape types
        query' = use $ fromList (Z :. typeLength) (padRight typeLength '\0' query)
        boolVector = all (\(T2 (I2 _ j) val) -> val == (query' !! j)) (indexed (use types))
        boolMatrix = generate (shape nc) (\(I2 i _) -> boolVector !! i)

        resVector = afst $ compact boolMatrix nc

        (I2 _ maxDepth ) = shape nc
        correctShape = I2 (size resVector `div` maxDepth) maxDepth

getParentCoordinates :: Acc (Matrix Int) -> Acc (Matrix Int)
getParentCoordinates nc = generate (I2 nodeCount depth) genElement
    where
        (I2 nodeCount depth) = shape nc
        genElement (I2 i j) =
            if j + 1 == depth || nc ! I2 i (j + 1) == 0
            then 0
            else nc ! I2 i j

findAncestorsOfType :: [Char] -> NCTree -> Acc (Array DIM1 Int)
findAncestorsOfType query tree@(nc, types, _) = closestAncestorVec
    where
        focusNodes = findNodesOfType query tree
        parentCoords = getParentCoordinates nc
        (I2 focusNodesCount maxDepth) = shape parentCoords
        (I2 nodeCount _) = shape nc
        
        focusNodesExt = replicate (lift (Z :. nodeCount :. All :. All)) focusNodes
        parentCoordsExt = replicate (lift (Z :. All :. focusNodesCount :. All)) parentCoords

        isAncestorMatrix = map boolToInt $ fold1 (&&) $ zipWith (\a b -> a == b || b == 0) parentCoordsExt focusNodesExt
        closestAncestorVec = fold1 max $ imap (\(I2 i j) e -> e*j) isAncestorMatrix

key :: NCTree -> Acc (Matrix Int) -> [(Acc (Vector Int), NCTree)]

exampleAst :: AST
exampleAst =
    Tree (ASTNode "Expr" "")
        [ Tree (ASTNode "Op" "/")
            [ Tree (ASTNode "Expr" "")
                [ Tree (ASTNode "Op" "+")
                    [ Tree (ASTNode "Expr" "")
                        [ Tree (ASTNode "Op" "*")
                            [ Tree (ASTNode "Var" "x") []
                            , Tree (ASTNode "Num" "2") []
                            ]
                        ]
                    , Tree (ASTNode "Num" "1") []
                    ]
                ]
            , Tree (ASTNode "Expr" "")
                [ Tree (ASTNode "Op" "+")
                    [ Tree (ASTNode "Num" "3") []
                    , Tree (ASTNode "Var" "y") []
                    ]
                ]
            ]
        ]
