{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- |
List-like functions on the inner dimension.
-}
module Data.Array.Accelerate.Utility.Sliced where

import qualified Data.Array.Accelerate.Utility.Lift.Exp as Exp
import Data.Array.Accelerate.Utility.Lift.Exp (expr)

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate
          (Exp, Acc, Array, Elt, Slice, Shape, DIM2, (:.)((:.)), (!), (?), )


length ::
   (Shape sh, Slice sh, Elt a) =>
   Acc (Array (sh:.Int) a) ->
   Exp Int
length = A.indexHead . A.shape

head ::
   (Shape sh, Slice sh, Elt a) =>
   Acc (Array (sh:.Int) a) ->
   Acc (Array sh a)
head xs = A.slice xs (A.constant $ A.Any:.(0::Int))

tail ::
   (Shape sh, Slice sh, Elt a) =>
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int) a)
tail xs =
   A.backpermute
      (Exp.modify (expr:.expr)
          (\(sh :. n) -> sh :. n-1)
          (A.shape xs))
      (Exp.modify (expr:.expr) $ \(ix:.k) -> ix :. k+1)
      xs

cons ::
   (Shape sh, Slice sh, Elt a) =>
   Acc (Array sh a) ->
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int) a)
cons x xs =
   A.generate
      (Exp.modify (expr:.expr)
          (\(sh :. n) -> sh :. n+1)
          (A.shape xs))
      (Exp.modify (expr:.expr) $
       \(ix:.k) -> k A.== 0 ? (x ! ix, xs ! A.lift (ix :. k-1)))

consExp ::
   (Shape sh, Slice sh, Elt a) =>
   Exp a ->
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int) a)
consExp x xs =
   A.generate
      (Exp.modify (expr:.expr)
          (\(z :. n) -> z :. n+1)
          (A.shape xs))
      (Exp.modify (expr:.expr) $
       \(ix:.k) -> k A.== 0 ? (x, xs ! A.lift (ix :. k-1)))


append3 ::
   (Shape sh, Slice sh, Elt a) =>
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int) a)
append3 x y z =
   let (sh :. n) = Exp.unlift (expr :. expr) $ A.shape x
   in  A.reshape (A.lift $ sh :. 3*n) $ stack3 x y z

stack3 ::
   (Shape sh, Slice sh, Elt a) =>
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a)
stack3 x y z =
   A.generate
      (Exp.modify (expr :. expr)
         (\(sh :. n) -> sh :. (3::Int) :. n)
         (A.shape x))
      (Exp.modify (expr :. expr :. expr) $
       \(globalIx :. k :. j) ->
          let ix = A.lift $ globalIx :. j
          in  flip (A.caseof k) (x ! ix) $
                 ((A.== 1), (y ! ix)) :
                 ((A.== 2), (z ! ix)) :
                 [])


take, drop ::
   (Shape sh, Slice sh, Elt a) =>
   Exp Int ->
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int) a)
take n arr =
   A.backpermute
      (Exp.modify (expr:.expr) (\(sh:._) -> sh:.n) $ A.shape arr)
      id arr

drop d arr =
   A.backpermute
      (Exp.modify (expr:.expr) (\(sh:.n) -> sh :. n - d) $ A.shape arr)
      (Exp.modify (expr:.expr) $ \(ix:.k) -> ix :. k + d)
      arr

pad ::
   (Shape sh, Slice sh, Elt a) =>
   Exp a -> Exp Int ->
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int) a)
pad x n arr =
   let sh:.m = Exp.unlift (expr:.expr) $ A.shape arr
   in  A.generate
          (A.lift $ sh:.n)
          (\ix -> m A.> A.indexHead ix ? (arr!ix, x))


{- |
@sliceVertical@ would be a simple 'A.reshape'.
-}
sliceHorizontal ::
   (Shape sh, Slice sh, Elt a) =>
   Exp DIM2 ->
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a)
sliceHorizontal nm arr =
   let _z:.n:.m = Exp.unlift (expr:.expr:.expr) nm
       sh = A.indexTail $ A.shape arr
   in  A.backpermute
          (A.lift $ sh :. n :. m)
          (Exp.modify (expr :. expr :. expr) $
           \(ix :. k :. j) -> ix :. n*j+k)
          arr

sieve ::
   (Shape sh, Slice sh, Elt a) =>
   Exp Int ->
   Exp Int ->
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int) a)
sieve m start arr =
   let sh:.n = Exp.unlift (expr:.expr) $ A.shape arr
   in  A.backpermute
          (A.lift $ sh :. div n m)
          (Exp.modify (expr :. expr) $
           \(ix :. j) -> ix :. m*j + start)
          arr
