{-# LANGUAGE TypeOperators #-}
module Demo where

import AccTree
import Tree
import Data.Array.Accelerate as A hiding ((++), (*), map)
import Data.Array.Accelerate.LLVM.Native (run)
import Data.List (intercalate)
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
iMatrix :: Exp Int -> Acc (Matrix Int)
iMatrix n = generate (I2 n n) (\(I2 i j) -> boolToInt (i A.>= j))

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunksOf n zs

runVec :: (Show e, Elt e) => Acc (Vector e) -> ([String] -> String) -> String
runVec arr f = f $ map show $ toList $ run arr

runMat :: (Show e, Elt e) => Acc (Matrix e) -> ([String] -> String) -> String
runMat arr f = unlines asList
    where
        computed = run arr
        (Z :. _ :. nCols) = arrayShape computed
        asList = map (f . map show) $ chunksOf nCols $ toList computed

latexTableLineFormatter :: Int -> [String] -> String 
latexTableLineFormatter level lines = concat (P.replicate level "\t") ++ intercalate " & " lines ++ " \\\\"