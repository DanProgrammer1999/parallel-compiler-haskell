{-# LANGUAGE TypeOperators #-}

module AccTest where

import Data.Array.Accelerate     as A
import qualified Prelude         as P

import Data.Array.Accelerate.Interpreter 

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

strTest :: P.String -> Array (Z :. Int) Char
strTest s = fromList (Z :. P.length s) s

getShape :: Elt e => Matrix e -> Exp Int
getShape array = let (Z :. rows :. cols) = unlift (shape (use array)) :: Z :. Exp Int :. Exp Int in rows

test1 :: Acc (Matrix (Float, Float))
test1 = zip a b

a = fill (I2 3 3) 1.0
b = fill (I2 5 4) 2.0