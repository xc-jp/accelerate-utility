{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
An alternative Shape class that allows to add new \"methods\",
but forbids to add new instances.
You can provide new \"methods\" by calling 'switch'
with a newtype around your function type.

For the general concept, see:
<https://www.haskell.org/haskellwiki/Closed_world_instances>
-}
module Data.Array.Accelerate.Utility.Shape (
   C(..),
   ) where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Exp, Z, (:.))


class
   (A.Shape sh, A.Slice sh, A.Elt sh, Eq sh,
    A.Plain sh ~ sh, A.Lift Exp sh) =>
      C sh where
   switch :: f Z -> (forall sh0. C sh0 => f (sh0 :. Int)) -> f sh

instance C Z where
   switch f _ = f

instance (C sh, i ~ Int) => C (sh:.i) where
   switch _ f = f
