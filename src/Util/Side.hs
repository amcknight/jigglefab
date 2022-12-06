module Util.Side
( Side (..)
, flipSide
) where

import GHC.Generics (Generic)
import Data.Serialize (Serialize)

data Side = Out | In deriving (Show, Eq, Ord, Generic)

instance Serialize Side

flipSide :: Side -> Side
flipSide In = Out
flipSide Out = In
