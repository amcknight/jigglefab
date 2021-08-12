{-# LANGUAGE DefaultSignatures #-}

module Chem
( Chem
, react, prereact, chemColor
, InnerChem
, innerReact, allowThru
) where

import Space
import Graphics.Gloss
import Pallet
import Pair
import Debug.Trace

class Show c => Chem c where
  react :: Sided c -> Sided c
  default react :: InnerChem c => Sided c -> Sided c
  react sc = if allowThru sc then flipSided sc 
    else
      let (cs, s) = sc
      in case s of
        Out -> sc
        In -> (innerReact cs, In)

  prereact :: Sided c -> P c
  default prereact :: Sided c -> P c
  prereact (es, _) = es

  chemColor :: c -> Pallet -> Color

class Chem c => InnerChem c where
  innerReact :: P c -> P c
  allowThru :: Sided c -> Bool
