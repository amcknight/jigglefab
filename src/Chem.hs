module Chem
( Chem (Chem)
, buildChem
, chem1, chem2, chem3
, hasUp, hasDown
, valence
, desire
) where

-- Chem Wanted Has
data Chem = Chem
  { wants :: Int 
  , has :: Int 
  } deriving Show

buildChem :: Int -> Chem
buildChem w = Chem w 0

chem1 :: Chem
chem1 = buildChem 1
chem2 :: Chem
chem2 = buildChem 2
chem3 :: Chem
chem3 = buildChem 3

hasUp :: Chem -> Chem
hasUp (Chem w h) = Chem w (h+1)
hasDown :: Chem -> Chem
hasDown (Chem w h) = Chem w (h-1)

valence :: Chem -> Int
valence (Chem want have) = want - have

desire :: Chem -> Ordering
desire (Chem want have)
  | have == want = EQ
  | have < want = LT
  | otherwise = GT
