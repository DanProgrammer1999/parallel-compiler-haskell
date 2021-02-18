{-# LANGUAGE TypeOperators #-}

module AccTree where

import Data.Array.Accelerate     as A
import qualified Prelude         as P
import Data.Array.Accelerate.Interpreter

data Tree a = Tree 
    { treeRoot     :: a
    , treeChildren :: [Tree a]
    }

data ASTNode = ASTNode 
    { nodeType  :: P.String
    , nodeValue :: P.String
    } deriving (P.Show)

type AST = Tree ASTNode
-- type VectorNode = (P.Int, P.String, P.String)
type String = Array (Z :. Int) Char

vectoriseTree :: AST -> [(P.Int, P.String, P.String)]
vectoriseTree tree = vectoriseTree' tree 0
    where
        vectoriseTree' (Tree root children) currLevel 
            = (currLevel, nodeType root, nodeValue root)
            : P.concatMap (\t -> vectoriseTree' t (currLevel + 1)) children

vectorTreeToArray :: [(P.Int, P.String, P.String)] -> (Array DIM1 Int, Matrix Char, Matrix Char)
vectorTreeToArray vTree = (depthAcc, typesAcc, valuesAcc)
    where 
        depthVector = P.map (\(d, _, _) -> d) vTree
        types = P.map (\(_, t, _) -> t) vTree
        values = P.map (\(_, _, v) -> v) vTree

        maxLength arr = P.maximum (P.map P.length arr)

        depthAcc = fromList (Z :. P.length depthVector) depthVector
        -- TODO this doesn't work! every input string needs to be padded to max length
        typesAcc = fromList (Z :. maxLength types :. P.length types) (P.concat types)
        valuesAcc = fromList (Z :. maxLength values :. P.length values) (P.concat values)


constructNodeCoordinates :: Acc (Array DIM1 Int) -> Acc (Array DIM2 Int) 
constructNodeCoordinates depthArray = undefined 
    where 
        maxDepth = maximum depthArray
        nodeCount = shape depthArray

        

        -- zeros = generate (nodeCount)

-- constructNodeCoordinates :: [Int] -> [NodeCoordinates]
-- constructNodeCoordinates depthVector = cutoffMatrix
--     where 
--         maxDepth = maximum depthVector
--         boolDepthMatrix = map (\d -> map (fromEnum . (== d)) [0 .. maxDepth]) depthVector
--         cumulativeMatrix = transpose $ map (scanl1 (+)) $ transpose boolDepthMatrix
--         cutoffMatrix = zipWith (\d nc -> take (d + 1) nc ++ replicate (maxDepth - d) 0) depthVector cumulativeMatrix

-- vectorToNCTree :: [VectorNode] -> NCTree
-- vectorToNCTree vectorTree = zip coordinates (map snd vectorTree)
--     where 
--         depthVector = map fst vectorTree
--         coordinates = constructNodeCoordinates depthVector

-- findNodesOfType :: String -> NCTree -> [NCNode] 
-- findNodesOfType nType = filter ((== nType) . nodeType . snd)

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
