module Valence.Valence
( Valence (Valence)
, vale
, hasUp, hasDown
, valence
, desire
, tie
, untie
) where

import Chem
import Space
import Pair
import Graphics.Gloss
import Pallet

data Valence = Valence
  { wants :: Int 
  , has :: Int 
  } deriving Show

instance Chem Valence where
  react (cs, In)
    | wantsLess cs = Exchange (untie cs, Out)
    | otherwise    = Exchange (cs, In)
  react (cs, Out)
    | wantsMore cs = Exchange (tie cs, In)
    | otherwise    = Exchange (cs, Out)
  prereact (cs, In) = tie cs
  prereact (cs, Out) = cs
  chemColor ch p = case desire ch of
    EQ -> getNeutral p
    GT -> getCool p
    LT -> getWarm p

vale :: Int -> Valence
vale w = Valence w 0

hasUp :: Valence -> Valence
hasUp (Valence w h) = Valence w (h+1)
hasDown :: Valence -> Valence
hasDown (Valence w h) = Valence w (h-1)

valence :: Valence -> Int
valence (Valence want have) = want - have

desire :: Valence -> Ordering
desire (Valence want have)
  | have == want = EQ
  | have < want = LT
  | otherwise = GT

wantsMore :: P Valence -> Bool
wantsMore (Valence w1 h1, Valence w2 h2) = w1 > h1 && w2 > h2

wantsLess :: P Valence -> Bool 
wantsLess (Valence w1 h1, Valence w2 h2) = w1 < h1 || w2 < h2

tie :: P Valence -> P Valence
tie = pmap hasUp

untie :: P Valence -> P Valence
untie = pmap hasDown
