module Chems
( Chems
, react
, Reactants (Reactants)
, Products (Products)
, contact
) where

import Chem
import Space

type Chems = (Chem, Chem)
data Reactants = Reactants Side Chems
data Products = Products Contact Chems

contact :: Products -> Contact
contact (Products contact _) = contact

react :: Reactants -> Products
react (Reactants Out cs)
  | wantsMore cs = Products Pass $ tie cs
  | otherwise    = Products Bounce cs
react (Reactants In cs)
  | wantsLess cs = Products Pass $ untie cs
  | otherwise    = Products Bounce cs

wantsMore :: Chems -> Bool 
wantsMore (Chem w1 h1, Chem w2 h2) = w1 > h1 && w2 > h2

wantsLess :: Chems -> Bool 
wantsLess (Chem w1 h1, Chem w2 h2) = w1 < h1 || w2 < h2

tie :: Chems -> Chems
tie (Chem w1 h1, Chem w2 h2) = (Chem w1 (h1+1), Chem w2 (h2+1))

untie :: Chems -> Chems
untie (Chem w1 h1, Chem w2 h2) = (Chem w1 (h1-1), Chem w2 (h2-1))