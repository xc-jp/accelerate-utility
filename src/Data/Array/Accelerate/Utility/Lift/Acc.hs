{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Array.Accelerate.Utility.Lift.Acc (
   Unlift,
   Unlifted,
   Tuple,
   unlift,
   modify,
   modify2,
   modify3,
   modify4,
   Acc(Acc), acc,
   Exp(Exp), expr,
   unliftPair,
   unliftTriple,
   unliftQuadruple,
   mapFst,
   mapSnd,
   singleton,
   the,
   ) where

import Data.Array.Accelerate.Utility.Lift.Exp (Exp(Exp), expr)
import qualified Data.Array.Accelerate as A

import qualified Data.Tuple.HT as Tuple
import Data.Tuple.HT (mapTriple)


{- |
This class is like 'Data.Array.Accelerate.Utility.Lift.Exp.Unlift'
but for the 'Acc' environment.
It allows you to unlift an 'Acc' of nested tuples
into tuples of 'Exp' and 'Acc' values.
It can be quite handy when working with 'A.acond' and 'A.awhile'.
It can also be useful in connection with running an @accelerate@ algorithm
at a certain backend,
like 'Data.Array.Accelerate.Interpreter.run1'.
But in this case you might prefer "Data.Array.Accelerate.Utility.Lift.Run".
-}
class (A.Arrays (Tuple pattern)) => Unlift pattern where
   type Unlifted pattern
   type Tuple pattern
   unlift :: pattern -> A.Acc (Tuple pattern) -> Unlifted pattern

modify ::
   (A.Lift A.Acc a, Unlift pattern) =>
   pattern ->
   (Unlifted pattern -> a) ->
   A.Acc (Tuple pattern) -> A.Acc (A.Plain a)
modify p f = A.lift . f . unlift p

modify2 ::
   (A.Lift A.Acc a, Unlift patternA, Unlift patternB) =>
   patternA ->
   patternB ->
   (Unlifted patternA -> Unlifted patternB -> a) ->
   A.Acc (Tuple patternA) -> A.Acc (Tuple patternB) -> A.Acc (A.Plain a)
modify2 pa pb f a b = A.lift $ f (unlift pa a) (unlift pb b)

modify3 ::
   (A.Lift A.Acc a, Unlift patternA, Unlift patternB, Unlift patternC) =>
   patternA ->
   patternB ->
   patternC ->
   (Unlifted patternA -> Unlifted patternB -> Unlifted patternC -> a) ->
   A.Acc (Tuple patternA) -> A.Acc (Tuple patternB) ->
   A.Acc (Tuple patternC) -> A.Acc (A.Plain a)
modify3 pa pb pc f a b c =
   A.lift $ f (unlift pa a) (unlift pb b) (unlift pc c)

modify4 ::
   (A.Lift A.Acc a,
    Unlift patternA, Unlift patternB, Unlift patternC, Unlift patternD) =>
   patternA ->
   patternB ->
   patternC ->
   patternD ->
   (Unlifted patternA -> Unlifted patternB ->
    Unlifted patternC -> Unlifted patternD -> a) ->
   A.Acc (Tuple patternA) -> A.Acc (Tuple patternB) ->
   A.Acc (Tuple patternC) -> A.Acc (Tuple patternD) -> A.Acc (A.Plain a)
modify4 pa pb pc pd f a b c d =
   A.lift $ f (unlift pa a) (unlift pb b) (unlift pc c) (unlift pd d)


instance (A.Arrays a) => Unlift (Acc a) where
   type Unlifted (Acc a) = A.Acc a
   type Tuple (Acc a) = a
   unlift _ = id

data Acc a = Acc

acc :: Acc a
acc = Acc


instance (A.Elt a) => Unlift (Exp a) where
   type Unlifted (Exp a) = A.Exp a
   type Tuple (Exp a) = A.Scalar a
   unlift _ = A.the


-- | like 'A.unit' in the 'Acc' environment
singleton :: (A.Elt e) => e -> A.Scalar e
singleton x = A.fromList A.Z [x]

-- | like 'A.the' in the 'Acc' environment
the :: (A.Elt e) => A.Scalar e -> e
the arr = A.indexArray arr A.Z


instance (Unlift pa, Unlift pb) => Unlift (pa,pb) where
   type Unlifted (pa,pb) = (Unlifted pa, Unlifted pb)
   type Tuple (pa,pb) = (Tuple pa, Tuple pb)
   unlift (pa,pb) ab =
      (unlift pa $ A.afst ab, unlift pb $ A.asnd ab)

instance
   (Unlift pa, Unlift pb, Unlift pc) =>
      Unlift (pa,pb,pc) where
   type Unlifted (pa,pb,pc) = (Unlifted pa, Unlifted pb, Unlifted pc)
   type Tuple (pa,pb,pc) = (Tuple pa, Tuple pb, Tuple pc)
   unlift (pa,pb,pc) =
      mapTriple (unlift pa, unlift pb, unlift pc) . A.unlift



unliftPair :: (A.Arrays a, A.Arrays b) => A.Acc (a,b) -> (A.Acc a, A.Acc b)
unliftPair = A.unlift

unliftTriple ::
   (A.Arrays a, A.Arrays b, A.Arrays c) => A.Acc (a,b,c) -> (A.Acc a, A.Acc b, A.Acc c)
unliftTriple = A.unlift

unliftQuadruple ::
   (A.Arrays a, A.Arrays b, A.Arrays c, A.Arrays d) =>
   A.Acc (a,b,c,d) -> (A.Acc a, A.Acc b, A.Acc c, A.Acc d)
unliftQuadruple = A.unlift


mapFst ::
   (A.Arrays a, A.Arrays b, A.Arrays c) =>
   (A.Acc a -> A.Acc b) -> A.Acc (a,c) -> A.Acc (b,c)
mapFst f = modify (acc,acc) $ Tuple.mapFst f

mapSnd ::
   (A.Arrays a, A.Arrays b, A.Arrays c) =>
   (A.Acc b -> A.Acc c) -> A.Acc (a,b) -> A.Acc (a,c)
mapSnd f = modify (acc,acc) $ Tuple.mapSnd f
