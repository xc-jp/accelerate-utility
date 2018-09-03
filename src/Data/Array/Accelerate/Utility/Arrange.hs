module Data.Array.Accelerate.Utility.Arrange (
   mapWithIndex,
   gather,
   scatter,
   ) where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Acc, Array, Exp)


mapWithIndex ::
   (A.Shape sh, A.Elt a, A.Elt b) =>
   (Exp sh -> Exp a -> Exp b) ->
   Acc (Array sh a) -> Acc (Array sh b)
mapWithIndex f xs =
   A.zipWith f (A.generate (A.shape xs) id) xs


gather ::
   (A.Shape ix, A.Shape ix', A.Elt ix', A.Elt a) =>
   Acc (Array ix ix') -> Acc (Array ix' a) -> Acc (Array ix a)
gather indices xs =
   A.map (xs A.!) indices

scatter ::
   (A.Shape ix, A.Shape ix', A.Elt ix', A.Elt a) =>
   (Exp a -> Exp a -> Exp a) ->
   Acc (Array ix ix') -> Acc (Array ix' a) ->
   Acc (Array ix a) -> Acc (Array ix' a)
scatter f indices deflt xs =
   A.permute f deflt (indices A.!) xs
