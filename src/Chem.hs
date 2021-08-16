{-# LANGUAGE DefaultSignatures #-}

module Chem
( Reactant(..)
, InReactant(..)
, Chem, react, prereact, chemColor
, InnerChem, innerReact, allowThru
) where

import Space
import Graphics.Gloss
import Pallet
import Pair
import Debug.Trace
import Data.Tuple

data Reactant c = LeftOnly c | RightOnly c | Exchange (Sided c) | Birth (Sided c) c

swapChems :: Reactant c -> Reactant c
swapChems (Exchange (cs, s)) = Exchange (swap cs, s)
swapChems (Birth (cs, s) c) = Birth (swap cs, s) c
swapChems r = r

data InReactant c = InLeftOnly c | InRightOnly c | InExchange (P c) | InBirth (P c) c
toReactant :: InReactant c -> Reactant c
toReactant (InLeftOnly c) = LeftOnly c 
toReactant (InRightOnly c) = RightOnly c 
toReactant (InExchange cs) = Exchange (cs, In)
toReactant (InBirth cs c) = Birth (cs, In) c

class Chem c where
  react :: Sided c -> Reactant c
  default react :: (Ord c, InnerChem c) => Sided c -> Reactant c
  react sc = if allowedThru
    then Exchange $ flipSided sc
    else case s of
      Out -> Exchange sc
      In -> reactant
    where
      (cs, s) = sc
      (c1, c2) = cs
      allowedThru = case compare c1 c2 of
        GT -> allowThru (swap cs, s)
        _ -> allowThru sc
      reactant = case compare c1 c2 of
        GT -> swapChems $ toReactant $ innerReact $ swap cs
        _ -> toReactant $ innerReact cs

  prereact :: Sided c -> P c
  default prereact :: Sided c -> P c
  prereact (es, _) = es

  chemColor :: c -> Pallet -> Color

class Chem c => InnerChem c where
  innerReact :: P c -> InReactant c
  allowThru :: Sided c -> Bool
