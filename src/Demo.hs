{-# LANGUAGE TypeOperators #-}
module Demo where

import AccTree
import Tree
import Data.Array.Accelerate as A hiding ((++), (*))
import Prelude as P

exampleAst :: AST
exampleAst =
    Tree (ASTNode "Expr" "1")
        [ Tree (ASTNode "Op" "/")
            [ Tree (ASTNode "Expr" "2")
                [ Tree (ASTNode "Op" "+")
                    [ Tree (ASTNode "Expr" "3")
                        [ Tree (ASTNode "Op" "*")
                            [ Tree (ASTNode "Var" "x") []
                            , Tree (ASTNode "Num" "2") []
                            ]
                        ]
                    , Tree (ASTNode "Num" "1") []
                    ]
                ]
            , Tree (ASTNode "Expr" "4")
                [ Tree (ASTNode "Op" "+")
                    [ Tree (ASTNode "Num" "3") []
                    , Tree (ASTNode "Var" "y") []
                    ]
                ]
            ]
        ]


tree :: Acc (Matrix Int, Matrix Char, Matrix Char)
nc :: Acc (Matrix Int)
types :: Acc (Matrix Char)
vals :: Acc (Matrix Char)
tree@(A.T3 nc types vals) = astToNCTree exampleAst

focusNodes :: Acc (Matrix Int)
focusNodes = findNodesOfType "Expr" tree

parentCoords :: Acc (Matrix Int)
parentCoords = getParentCoordinates nc

closestFocusAncestors :: Acc (Matrix Int)
closestFocusAncestors = findAncestorsOfType "Expr" tree

-- A matrix where element_(i, j) is 1 if i >= j
iMatrix :: A.Exp Int -> A.Acc (A.Matrix Int)
iMatrix n = generate (A.I2 n n) (\(A.I2 i j) -> boolToInt (i A.>= j))
