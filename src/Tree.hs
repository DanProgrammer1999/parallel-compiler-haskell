module Tree where

import Data.List (transpose)

data Tree a = Tree 
    { treeRoot     :: a
    , treeChildren :: [Tree a]
    }

data ASTNode = ASTNode 
    { nodeType  :: String
    , nodeValue :: String
    } deriving (Show)

type AST = Tree ASTNode

type VectorNode = (Int, ASTNode)
type VectorTree = [VectorNode]

type NodeCoordinates = [Int]
type NCNode = (NodeCoordinates, ASTNode)
type NCTree = [NCNode]

vectoriseTree :: AST -> [VectorNode]
vectoriseTree tree = vectoriseTree' tree 0
    where
        vectoriseTree' (Tree root children) currLevel = 
            (currLevel, root) : concatMap (\t -> vectoriseTree' t (currLevel + 1)) children

constructNodeCoordinates :: [Int] -> [NodeCoordinates]
constructNodeCoordinates depthVector = cutoffMatrix
    where 
        maxDepth = maximum depthVector
        boolDepthMatrix = map (\d -> map (fromEnum . (== d)) [0 .. maxDepth]) depthVector
        cumulativeMatrix = transpose $ map (scanl1 (+)) $ transpose boolDepthMatrix
        cutoffMatrix = zipWith (\d nc -> take (d + 1) nc ++ replicate (maxDepth - d) 0) depthVector cumulativeMatrix

vectorToNCTree :: [VectorNode] -> NCTree
vectorToNCTree vectorTree = zip coordinates (map snd vectorTree)
    where 
        depthVector = map fst vectorTree
        coordinates = constructNodeCoordinates depthVector

findNodesOfType :: String -> NCTree -> [NCNode] 
findNodesOfType nType = filter ((== nType) . nodeType . snd)

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

exampleNCTree = vectorToNCTree $ vectoriseTree exampleAst

test1 = fmap (print . fst) exampleNCTree
test2 = mapM_ print exampleNCTree
test3 = mapM_ print $ findNodesOfType "Expr" exampleNCTree

-- newtype ASTNode = ASTNode { getNode :: String }
