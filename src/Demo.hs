{-# LANGUAGE TypeOperators #-}
module Demo where

import AccTree
import Data.Array.Accelerate as A
import Prelude as P (putStrLn, unlines, map, show, replicate, repeat, concat, sum)

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


buildNLevelAST :: Int -> AST
buildNLevelAST 0 = astLeafNode "Num" "42"
buildNLevelAST n =
    Tree (ASTNode "Expr" "")
        [
            Tree (ASTNode "Op" "+")
            [
                buildNLevelAST (n - 1),
                buildNLevelAST (n - 1)
            ]
        ]

treeSize :: AST -> Int
treeSize = calcTreeSize 1
    where
        calcTreeSize count (Tree _ children) = count + P.sum (P.map (calcTreeSize count) children)

printDepthVec = putStrLn $ unlines $ P.map show $ vectoriseTree exampleAst

tree@(nc, types, vals) = astToNCTree exampleAst
focusNodes = findNodesOfType "Expr" tree
parentCoords = getParentCoordinates nc
closestFocusAncestors = findAncestorsOfType "Expr" tree

a :: Acc (Matrix Int)
a = use $ A.fromList (Z :. 3 :. 4) [1..]

b :: Acc (Matrix Int)
b = A.replicate (lift (Z :. (5 :: Int) :. All)) $ use $ fromList (Z :. 5) (P.repeat 5)

c :: Acc (Matrix Int)
c = A.replicate (lift (Z :. (5 :: Int) :. All)) $ use $ fromList (Z :. 5) [1..]

iMatrix :: Exp Int -> Acc (Matrix Int)
iMatrix n = generate (I2 n n) (\(I2 i j) -> boolToInt (i >= j))

test = innerProduct (\a b -> a == b || b == 0) (&&) parentCoords focusNodes