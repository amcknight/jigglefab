module Chems
( Chems
, react
, tie
, untie
, Reactants
, Products
) where

import Chem
import Space
import Pair

type Chems = (Chem, Chem)
type SidedChems = (Side, Chems)
type Reactants = SidedChems
type Products = SidedChems

react :: Reactants -> Products
react (Out, cs)
  | wantsMore cs = (In, tie cs)
  | otherwise    = (Out, cs)
react (In, cs)
  | wantsLess cs = (Out, untie cs)
  | otherwise    = (In, cs)

wantsMore :: Chems -> Bool 
wantsMore (Chem w1 h1, Chem w2 h2) = w1 > h1 && w2 > h2

wantsLess :: Chems -> Bool 
wantsLess (Chem w1 h1, Chem w2 h2) = w1 < h1 || w2 < h2

tie :: Chems -> Chems
tie = bimap hasUp

untie :: Chems -> Chems
untie = bimap hasDown