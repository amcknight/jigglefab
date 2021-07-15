module Chem
( Chem (Chem)
, valence
, desire
) where

import Space

-- Chem Wanted Has
data Chem = Chem Int Int deriving Show
valence :: Chem -> Int
valence (Chem want have) = want - have
desire :: Chem -> Ordering
desire (Chem want have)
  | have == want = EQ
  | have < want = LT
  | otherwise = GT
