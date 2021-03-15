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

a = use (fromList (Z:.10) [0..] :: Vector Int)

aExt = replicate (lift (Z :. All :. (2 :: Int))) a
aExt' = replicate (lift (Z :. (2 :: Int) :. All :. All)) aExt
