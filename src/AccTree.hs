{-# LANGUAGE TypeOperators, ViewPatterns, FlexibleContexts, RebindableSyntax #-}

module AccTree where

import Data.Array.Accelerate     as A
import qualified Prelude         as P
import Data.Array.Accelerate.Interpreter

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
type NCTree = Acc(Matrix (Int, Char, Char))

vectoriseTree :: AST -> [(P.Int, P.String, P.String)]
vectoriseTree tree = vectoriseTree' tree 0
    where
        vectoriseTree' (Tree root children) currLevel 
            = (currLevel, nodeType root, nodeValue root)
            : P.concatMap (\t -> vectoriseTree' t (currLevel + 1)) children

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

vectorToNCTree :: VectorTree -> Acc (Matrix Int, Matrix Char, Matrix Char)
vectorToNCTree (depthVec, types, values) = 
    let 
        depthVec' = use depthVec
        types' = use types
        values' = use values
        nodeCoordinates = constructNodeCoordinates depthVec'
    in lift (nodeCoordinates, types', values')

astToNCTree :: AST -> Acc (Matrix Int, Matrix Char, Matrix Char)
astToNCTree = vectorToNCTree . treeToAccelerate . vectoriseTree 

-- findNodesOfType :: String -> NCTree -> [NCNode] 
-- findNodesOfType nType = filter ((== nType) . nodeType . snd)

findNodesOfType :: [Char] -> NCTree -> Matrix Int
findNodesOfType query (unzip3 -> (nc, types, vals)) = undefined
    where
        (I2 _ typeLength) = unlift $ shape types
        toPad :: Exp (Plain Char)
        toPad = lift '\0'

        -- query' = lift3 padRight typeLength toPad (lift query)

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
