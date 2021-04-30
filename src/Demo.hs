{-# LANGUAGE TypeOperators #-}
module Demo where

import AccTree
import Tree
import Data.Array.Accelerate as A hiding ((++), (*))
import Prelude as P

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
buildNLevelAST n
    | n P.<= 0 = astLeafNode "Num" "42"
    | otherwise =
        Tree (ASTNode "Expr" "")
        [
            Tree (ASTNode "Op" "+")
            [
                buildNLevelAST (n - 3),
                buildNLevelAST (n - 3)
            ]
        ]

printDepthVec = putStrLn $ unlines $ P.map show $ vectoriseTree exampleAst

tree@(nc, types, vals) = astToNCTree exampleAst
focusNodes = findNodesOfType "Expr" tree
parentCoords = getParentCoordinates nc
closestFocusAncestors = findAncestorsOfType "Expr" tree

a :: A.Acc (A.Matrix Int)
a = use $ A.fromList (Z :. 3 :. 4) [1..]

b :: A.Acc (A.Matrix A.Int)
b = A.replicate (A.lift (Z :. (5 :: Int) :. All)) $ A.use $ A.fromList (Z :. 5) (P.repeat 5)

c :: A.Acc (A.Matrix Int)
c = A.replicate (lift (Z :. (5 :: Int) :. All)) $ use $ fromList (Z :. 5) [1..]

iMatrix :: A.Exp Int -> A.Acc (A.Matrix Int)
iMatrix n = generate (A.I2 n n) (\(A.I2 i j) -> boolToInt (i A.>= j))

test = innerProduct (\a b -> a A.== b A.|| b A.== 0) (A.&&) parentCoords focusNodes