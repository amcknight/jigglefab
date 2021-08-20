module Electro.Electro
( Electro (Dormant, Active)
) where

import Chem
import Pallet

data Electro = Dormant | Active | Tired Int deriving (Show, Eq, Ord)

instance Chem Electro where
  chemColor Dormant = getNeutral
  chemColor Active = getHot
  chemColor (Tired n) = getCool

instance InnerChem Electro where
  innerReact (Dormant, Active) = InExchange (Active, Tired 3)
  innerReact (Dormant, Tired 0) = InExchange (Dormant, Dormant)
  innerReact (Dormant, Tired n) = InExchange (Dormant, Tired (n-1))
  innerReact (Tired 0, Tired 0) = InExchange (Dormant, Dormant)
  innerReact (Tired 0, Tired n) = InExchange (Dormant, Tired (n-1))
  innerReact (Tired m, Tired n) = InExchange (Tired (m-1), Tired (n-1))
  innerReact es = InExchange es
  allowThru _ = False
