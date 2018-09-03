{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- |
List-like functions on the next-to-innermost dimension.
-}
module Data.Array.Accelerate.Utility.Sliced1 where

import qualified Data.Array.Accelerate.Utility.Lift.Exp as Exp
import Data.Array.Accelerate.Utility.Lift.Exp (expr)

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate
          (Exp, Acc, Array, Elt, (:.)((:.)), Slice, Shape, (!), (?), )


length ::
   (Shape sh, Slice sh, Elt a) =>
   Acc (Array (sh:.Int:.Int) a) ->
   Exp Int
length = A.indexHead . A.indexTail . A.shape

head ::
   (Shape sh, Slice sh, Elt a) =>
   Acc (Array (sh:.Int:.Int) a) ->
   Acc (Array (sh:.Int) a)
head xs = A.slice xs (A.constant $ A.Any:.(0::Int):.A.All)

tail ::
   (Shape sh, Slice sh, Elt a) =>
   Acc (Array (sh:.Int:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a)
tail xs =
   A.backpermute
      (Exp.modify (expr:.expr:.expr)
          (\(sh :. n :. m) -> sh :. n-1 :. m)
          (A.shape xs))
      (Exp.modify (expr:.expr:.expr) $ \(ix:.k:.j) -> ix :. k+1 :. j)
      xs

cons ::
   (Shape sh, Slice sh, Elt a) =>
   Acc (Array (sh:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a)
cons x xs =
   A.generate
      (Exp.modify (expr:.expr:.expr)
          (\(sh :. n :. m) -> sh :. n+1 :. m)
          (A.shape xs))
      (Exp.modify (expr:.expr:.expr) $
       \(ix:.k:.j) ->
          k A.== 0 ? (x ! A.lift (ix:.j), xs ! A.lift (ix :. k-1 :. j)))

{- |
The outer and innermost dimensions must match.
Otherwise you may or may not get out-of-bound errors.
-}
append ::
   (Shape sh, Slice sh, Elt a) =>
   Acc (Array (sh:.Int:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a)
append x y =
   let ( shx:.nx:.lenx) = Exp.unlift (expr:.expr:.expr) $ A.shape x
       (_shy:.ny:.leny) = Exp.unlift (expr:.expr:.expr) $ A.shape y
   in  A.generate (A.lift $ shx :. nx+ny :. max lenx leny) $
       Exp.modify (expr:.expr:.expr) $ \(ix:.k:.j) ->
          nx A.> k ? (x ! A.lift (ix:.k:.j), y ! A.lift (ix:.k-nx:.j))

append3 ::
   (Shape sh, Slice sh, Elt a) =>
   Acc (Array (sh:.Int:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a)
append3 x y z =
   let (sh :. n :. m) = Exp.unlift (expr :. expr :. expr) $ A.shape x
   in  A.reshape (A.lift $ sh :. 3*n :. m) $ stack3 x y z

stack3 ::
   (Shape sh, Slice sh, Elt a) =>
   Acc (Array (sh:.Int:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a) ->
   Acc (Array (sh:.Int:.Int:.Int) a)
stack3 x y z =
   A.generate
      (Exp.modify (expr :. expr :. expr)
         (\(sh :. n :. m) -> sh :. (3::Int) :. n :. m)
         (A.shape x))
      (Exp.modify (expr :. expr :. expr :. expr) $
       \(globalIx :. k :. j :. i) ->
          let ix = A.lift $ globalIx :. j :. i
          in  flip (A.caseof k) (x ! ix) $
                 ((A.== 1), (y ! ix)) :
                 ((A.== 2), (z ! ix)) :
                 [])


take, drop ::
   (Shape sh, Slice sh, Elt a) =>
   Exp Int ->
   Acc (Array (sh:.Int:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a)
take n arr =
   A.backpermute
      (Exp.modify (expr:.expr:.expr) (\(sh:._:.m) -> sh:.n:.m) $ A.shape arr)
      id arr

drop d arr =
   A.backpermute
      (Exp.modify (expr:.expr:.expr)
         (\(sh:.n:.m) -> sh :. n - d :. m) $ A.shape arr)
      (Exp.modify (expr:.expr:.expr) $
       \(ix:.k:.j) -> ix :. k + d :. j)
      arr

sieve ::
   (Shape sh, Slice sh, Elt a) =>
   Exp Int ->
   Exp Int ->
   Acc (Array (sh:.Int:.Int) a) ->
   Acc (Array (sh:.Int:.Int) a)
sieve fac start arr =
   let sh:.n:.m = Exp.unlift (expr:.expr:.expr) $ A.shape arr
   in  A.backpermute
          (A.lift $ sh :. div n fac :. m)
          (Exp.modify (expr :. expr :. expr) $
           \(ix :. k :. j) -> ix :. fac*k+start :. j)
          arr
