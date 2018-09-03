module Data.Array.Accelerate.Utility.Loop (
   nest,
   nestLog2,
   ) where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Acc, Exp)



nest, _nest ::
   (A.Arrays a) =>
   Exp Int -> (Acc a -> Acc a) -> Acc a -> Acc a
nest n0 f x0 =
   A.asnd $
   A.awhile
      (A.map (A.> 0) . A.afst)
      (A.lift . (\(n, x) -> (A.map (subtract 1) n, f x)) . A.unlift)
      (A.lift (A.unit n0, x0))

_nest n0 f x0 =
   A.asnd $
   A.awhile
      (A.unit . (A.> 0) . A.the . A.afst)
      (A.lift . (\(n, x) -> (A.unit $ A.the n - 1, f x)) . A.unlift)
      (A.lift (A.unit n0, x0))


nestLog2 ::
   (A.Arrays a) =>
   Exp Int -> (Acc a -> Acc a) -> Acc a -> Acc a
nestLog2 n0 f x0 =
   A.asnd $
   A.awhile
      (A.map (A.> 1) . A.afst)
      (A.lift . (\(n, x) -> (A.map (flip div 2 . (1+)) n, f x)) . A.unlift)
      (A.lift (A.unit n0, x0))
