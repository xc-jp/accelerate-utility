{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Array.Accelerate.Utility.Ord where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate
          (Exp, Acc, Array, Elt, Slice, Shape, Scalar, (?), )


argmin ::
   (Elt a, Elt b, A.Ord a) =>
   Exp (a, b) -> Exp (a, b) -> Exp (a, b)
argmin x y  =  A.fst x A.<= A.fst y ? (x,y)

argminimum ::
   (Slice sh, Shape sh, Elt a, Elt b, A.Ord a) =>
   Acc (Array sh (a, b)) -> Acc (Scalar (a, b))
argminimum = A.fold1All argmin


argmax ::
   (Elt a, Elt b, A.Ord a) =>
   Exp (a, b) -> Exp (a, b) -> Exp (a, b)
argmax x y  =  A.fst x A.<= A.fst y ? (y,x)

argmaximum ::
   (Slice sh, Shape sh, Elt a, Elt b, A.Ord a) =>
   Acc (Array sh (a, b)) -> Acc (A.Scalar (a, b))
argmaximum = A.fold1All argmax
