module Chem.Valence
( Valence (Valence)
, vale
, hasUp, hasDown
, valence
, desire
, tie, untie
, ballWall
, twoBall
, twoBallInner
, threeBall
, threeBallInner
, fourBall
, fourBallInner
, sevenBall
) where

import Chem
import Geometry.Space
import Pair
import Color
import Struct
import Orb
import Geometry.Vector
import StructLibrary
import Wall
import GHC.Generics
import Enumer

data Valence = Valence
  { wants :: Int 
  , has :: Int 
  } deriving (Show, Generic)

instance Chem Valence where
  react (cs, In)
    | wantsLess cs = Exchange (untie cs, Out)
    | otherwise    = Exchange (cs, In)
  react (cs, Out)
    | wantsMore cs = Exchange (tie cs, In)
    | otherwise    = Exchange (cs, Out)
  prereact (cs, In) = tie cs
  prereact (cs, Out) = cs
  chemColor ch = case desire ch of
    EQ -> grey
    GT -> red
    LT -> blue

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

---------------

ballWall :: Struct Valence
ballWall = box (-10,-10) (10, 10) <> orbStruct (Orb zeroV (vale 1))

twoBall :: Struct Valence
twoBall =
  orbStruct (Orb zeroV (vale 1)) <>
  orbStruct (Orb (20, 0.6) (vale 1))

twoBallInner :: Struct Valence
twoBallInner = 
  orbStruct (Orb (0.0,0.5) (vale 1)) <>
  orbStruct (Orb (0.0,0.0) (vale 2))

threeBall :: Struct Valence
threeBall =
  wallStruct (VLine 1200) <>
  orbStruct (Orb ( 0, -2) (vale 1)) <>
  orbStruct (Orb (20, -3) (vale 1)) <>
  orbStruct (Orb (10, 20) (vale 1))

threeBallInner :: Struct Valence
threeBallInner =
  orbStruct (Orb ( 0.5, 0.02) (vale 1)) <>
  orbStruct (Orb ( 1.1,-0.01) (vale 0)) <>
  orbStruct (Orb ( 1.9, 0.01) (vale 2))

fourBall :: Struct Valence
fourBall =
  orbStruct (Orb ( 0, -2) (vale 1)) <>
  orbStruct (Orb (20, -3) (vale 1)) <>
  orbStruct (Orb (10, 20) (vale 1)) <>
  orbStruct (Orb (14, 20) (vale 1))

fourBallInner :: Struct Valence
fourBallInner = 
  orbStruct (Orb zeroV (vale 1)) <>
  orbStruct (Orb ( 0.2, 0.6) (vale 2)) <>
  orbStruct (Orb (-0.3, 0.3) (vale 3)) <>
  orbStruct (Orb (-0.7, 1.1) (vale 4))

sevenBall :: Struct Valence
sevenBall = 
  orbStruct (Orb (-0.54, 0.51) (vale 1)) <>
  orbStruct (Orb (-0.36, 0.05) (vale 1)) <>
  orbStruct (Orb ( 0.20, 0.70) (vale 2)) <>
  orbStruct (Orb (-0.73, 1.14) (vale 1)) <>
  orbStruct (Orb (-0.27, 0.68) (vale 3)) <>
  orbStruct (Orb ( 0.3, 0.6) (vale 4)) <>
  orbStruct (Orb ( 2.7,-1.0) (vale 4)) <>
  orbStruct (Orb (-2.54,-1.51) (vale 1)) <>
  orbStruct (Orb (-3.36,-3.05) (vale 1)) <>
  orbStruct (Orb (-1.2, 3.7) (vale 2)) <>
  orbStruct (Orb (0.73, 2.14) (vale 1)) <>
  orbStruct (Orb (-2.27, -1.68) (vale 3)) <>
  orbStruct (Orb ( 3.3, -0.6) (vale 3)) <>
  orbStruct (Orb ( 3.7,-2.0) (vale 4))
