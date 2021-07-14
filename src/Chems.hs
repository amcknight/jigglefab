module Chems
( Chems
, react
, Reactants (Reactants)
, Products (Products)
) where

import Chem
import Space

type Chems = (Chem, Chem)
data Reactants = Reactants Side Chems
data Products = Products Hit Chems

react :: Reactants -> Products
react (Reactants Out (Chem w1 h1, Chem w2 h2))
  | w1 > h1 && w2 > h2 = Products Bounce (Chem w1 (h1+1), Chem w2 (h2+1))
  | otherwise          = Products Pass (Chem w1 h1, Chem w2 h2)
react (Reactants In (Chem w1 h1, Chem w2 h2))
  | w1 < h1 || w2 < h2 = Products Pass (Chem w1 (h1-1), Chem w2 (h2-1))
  | otherwise          = Products Bounce (Chem w1 h1, Chem w2 h2)
