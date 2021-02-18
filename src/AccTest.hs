{-# LANGUAGE TypeOperators #-}

module AccTest where

import Data.Array.Accelerate     as A
import qualified Prelude         as P

dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)

strTest :: P.String -> Array (Z :. Int) Char
strTest s = fromList (Z :. P.length s) s

getShape :: Elt e => Array ((Z :. Int) :. Int) e -> Exp Int
getShape array = let (Z :. rows :. cols) = unlift (shape (use array)) :: Z :. Exp Int :. Exp Int in rows
