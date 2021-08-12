module Core.Core
( Core (..)
) where

import Chem
import Pallet
import Space


data Core = Active | Dormant | Creator | Sensitized deriving Show

instance Chem Core where
  chemColor Dormant = getNeutral
  chemColor Active = getHot
  chemColor Creator = getWarm
  chemColor Sensitized = getCold

instance InnerChem Core where
  innerReact (Active, Dormant) = (Dormant, Active)
  innerReact (Dormant, Active) = (Active, Dormant)
  innerReact (Sensitized, Active) = (Active, Dormant)
  innerReact (Active, Sensitized) = (Dormant, Active)
  innerReact (Creator, Active) = (Dormant, Dormant)
  innerReact (Active, Creator) = (Dormant, Dormant)
  innerReact cs = cs
  
  allowThru ((Sensitized, Active), Out) = True
  allowThru ((Active, Sensitized), Out) = True
  allowThru sc = False
