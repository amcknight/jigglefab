module Util.Enumer
( Enumer(..)
) where

import GHC.Generics
import Util.Side

deriving instance Enumer Side

class GEnumer f where
  gvals :: [f x]

class Enumer a where
  vals :: [a]
  default vals :: (Generic a, GEnumer (Rep a)) => [a]
  vals = fmap to gvals

instance (Enumer a) => GEnumer (K1 _1 a) where
  gvals = fmap K1 vals

instance GEnumer f => GEnumer (M1 _1 _2 f) where
  gvals = fmap M1 gvals

instance GEnumer U1 where
  gvals = [U1]

instance (GEnumer f, GEnumer g) => GEnumer (f :+: g) where
  gvals = fmap L1 gvals ++ fmap R1 gvals

instance (GEnumer f, GEnumer g) => GEnumer (f :*: g) where
  gvals = (:*:) <$> gvals <*> gvals
