{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{- |
Simplify running @accelerate@ functions
with multiple curried array and expression arguments.
-}
module Data.Array.Accelerate.Utility.Lift.Run (
   C(..), with,
   Argument(..),
   ) where

import qualified Data.Array.Accelerate.Utility.Lift.Acc as Acc

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Acc, Exp, Z, (:.))

import Data.Tuple.HT (mapPair, mapTriple)


merge ::
   (A.Arrays a, A.Arrays packed) =>
   (Acc packed -> b) ->
   (Acc a -> b -> c) -> (Acc (a,packed) -> c)
merge unpack f arr = f (A.afst arr) (unpack $ A.asnd arr)

_mergeAcc ::
   (A.Arrays a, A.Arrays b) =>
   (Acc a -> Acc b -> c) -> (Acc (a,b) -> c)
_mergeAcc f arr = f (A.afst arr) (A.asnd arr)

_mergeExp ::
   (A.Elt a, A.Arrays b) =>
   (Exp a -> Acc b -> c) -> (Acc (A.Scalar a, b) -> c)
_mergeExp f arr = f (A.the $ A.afst arr) (A.asnd arr)

_mergeExpR ::
   (A.Arrays a, A.Elt b) =>
   (Acc a -> Exp b -> c) -> (Acc (a, A.Scalar b) -> c)
_mergeExpR f arr = f (A.afst arr) (A.the $ A.asnd arr)


split ::
   (A.Arrays a, A.Arrays packed) =>
   (unpacked -> packed) ->
   ((a, packed) -> c) -> (a -> unpacked -> c)
split pack f a b = f (a, pack b)

_splitAcc :: ((a,b) -> c) -> (a -> b -> c)
_splitAcc = curry

_splitExp :: (A.Elt a) => ((A.Scalar a, b) -> c) -> (a -> b -> c)
_splitExp f a b = f (Acc.singleton a, b)

_splitExpR :: (A.Elt b) => ((a, A.Scalar b) -> c) -> (a -> b -> c)
_splitExpR f a b = f (a, Acc.singleton b)



{- |
If you have a function:

> f :: Exp a -> (Acc b, Acc c) -> (Acc d, Acc e)

you cannot run this immediately using 'Data.Array.Accelerate.Interpreter.run1',
since @run1@ expects a function
with a single 'Acc' parameter and a single 'Acc' result.
Using the 'with' function you can just run @f@ as is:

> with run1 f :: a -> (b,c) -> (d,e)
-}
{-
(Acc ((a,b),c)) -> Acc d) -> (((a,b),c) -> d)
(Acc (a,b) -> Acc c -> Acc d) -> ((a,b) -> c -> d)
(Acc a -> Acc b -> Acc c -> Acc d) -> (a -> b -> c -> d)
-}
class C f where
   type Arguments a f
   type Result f
   type Plain f
   with1 ::
      (A.Arrays a) =>
      ((Acc (Arguments a f) -> Acc (Result f)) ->
       (Arguments a f -> Result f)) ->
      (Acc a -> f) -> a -> Plain f

with ::
   (C f) =>
   ((Acc (Arguments () f) -> Acc (Result f)) ->
    (Arguments () f -> Result f)) ->
   f -> Plain f
with run f = with1 run (const f) ()


instance C (Acc r) where
   type Arguments a (Acc r) = a
   type Result (Acc r) = r
   type Plain (Acc r) = r
   with1 run f = run f

instance
   (A.Lift Acc r, A.Arrays (A.Plain r),
    A.Lift Acc s, A.Arrays (A.Plain s)) =>
      C (r,s) where
   type Arguments a (r,s) = a
   type Result (r,s) = (A.Plain r, A.Plain s)
   type Plain (r,s) = (A.Plain r, A.Plain s)
   with1 run f = run (A.lift . f)

instance
   (A.Lift Acc r, A.Arrays (A.Plain r),
    A.Lift Acc s, A.Arrays (A.Plain s),
    A.Lift Acc t, A.Arrays (A.Plain t)) =>
      C (r,s,t) where
   type Arguments a (r,s,t) = a
   type Result (r,s,t) = (A.Plain r, A.Plain s, A.Plain t)
   type Plain (r,s,t) = (A.Plain r, A.Plain s, A.Plain t)
   with1 run f = run (A.lift . f)



instance (Argument arg, C f) => C (arg -> f) where
   type Arguments a (arg -> f) = Arguments (a, Packed arg) f
   type Result (arg -> f) = Result f
   type Plain (arg -> f) = Unpacked arg -> Plain f
   with1 run f =
      case tunnel of
         (pack, unpack) ->
            split pack (with1 run $ merge unpack f)


class (A.Arrays (Packed a)) => Argument a where
   type Packed a
   type Unpacked a
   tunnel :: (Unpacked a -> Packed a, Acc (Packed a) -> a)

instance (A.Arrays a) => Argument (Acc a) where
   type Packed (Acc a) = a
   type Unpacked (Acc a) = a
   tunnel = (id, id)

instance (A.Elt a) => Argument (Exp a) where
   type Packed (Exp a) = A.Scalar a
   type Unpacked (Exp a) = a
   tunnel = (Acc.singleton, A.the)

instance (Argument a, Argument b) => Argument (a,b) where
   type Packed (a,b) = (Packed a, Packed b)
   type Unpacked (a,b) = (Unpacked a, Unpacked b)
   tunnel =
      case (tunnel, tunnel) of
         ((packA, unpackA), (packB, unpackB)) ->
            (mapPair (packA,packB),
             mapPair (unpackA,unpackB) . A.unlift)

instance (Argument a, Argument b, Argument c) => Argument (a,b,c) where
   type Packed (a,b,c) = (Packed a, Packed b, Packed c)
   type Unpacked (a,b,c) = (Unpacked a, Unpacked b, Unpacked c)
   tunnel =
      case (tunnel, tunnel, tunnel) of
         ((packA, unpackA), (packB, unpackB), (packC, unpackC)) ->
            (mapTriple (packA,packB,packC),
             mapTriple (unpackA,unpackB,unpackC) . A.unlift)


instance Argument Z where
   type Packed Z = A.Scalar Z
   type Unpacked Z = Z
   tunnel = (Acc.singleton, A.unlift . A.the)

instance
   (A.Unlift Exp a, A.Lift Exp a, A.Slice (A.Plain a), b ~ Exp Int) =>
      Argument (a:.b) where
   type Packed (a:.b) = A.Scalar (A.Plain a :. Int)
   type Unpacked (a:.b) = A.Plain a :. Int
   tunnel = (Acc.singleton, A.unlift . A.the)

