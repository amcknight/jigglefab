module Valence.Valence
( Valence (Valence)
, Valences
, vale
, hasUp, hasDown
, valence
, desire
, tie
, untie
) where

import Chemy
import Space
import Pair
import Graphics.Gloss

data Valence = Valence
  { wants :: Int 
  , has :: Int 
  } deriving Show

instance Chemy Valence where
  react (In, cs)
    | wantsLess cs = (Out, untie cs)
    | otherwise    = (In, cs)
  react (Out, cs)
    | wantsMore cs = (In, tie cs)
    | otherwise    = (Out, cs)
  prereact (In, cs) = tie cs
  prereact (Out, cs) = cs
  chemColor ch = case desire ch of
    EQ -> greyN 0.5
    GT -> red
    LT -> green

type Valences = Chemies Valence

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

wantsMore :: Valences -> Bool
wantsMore (Valence w1 h1, Valence w2 h2) = w1 > h1 && w2 > h2

wantsLess :: Valences -> Bool 
wantsLess (Valence w1 h1, Valence w2 h2) = w1 < h1 || w2 < h2

tie :: Valences -> Valences
tie = bimap hasUp

untie :: Valences -> Valences
untie = bimap hasDown
