{-# LANGUAGE TypeOperators #-}

module AccTest where

import Data.Array.Accelerate  as A
import qualified Prelude      as P


dotp :: Acc (Vector Float) -> Acc (Vector Float) -> Acc (Scalar Float)
dotp xs ys = A.fold (+) 0 (A.zipWith (*) xs ys)