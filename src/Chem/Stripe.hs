module Chem.Stripe
( Stripe (..)
, longStruct
) where

import Model.Chem
import Color
import Geometry.Vector
import Model.Struct
import Model.Orb
import Util.Enumer
import GHC.Generics
import Geometry.Circle

data Trip = A | B | C deriving (Show, Eq, Ord, Generic, Enumer)
data Stripe = No Trip | Yes Trip deriving (Show, Eq, Ord, Generic, Enumer)

instance Chem Stripe where
  chemColor (No A) = light grey
  chemColor (No B) = grey
  chemColor (No C) = dark grey
  chemColor (Yes _) = red

instance InnerChem Stripe where
  innerReact (No A, Yes C) = InExchange (Yes A, No C)
  innerReact (No B, Yes A) = InExchange (Yes B, No A)
  innerReact (No C, Yes B) = InExchange (Yes C, No B)
  innerReact cs = InExchange cs

longStruct :: Struct Stripe
longStruct = string0 <>
    string1 <>
    string2 <>
    string3 <>
    string4 <>
    string5 <>
    string6 <>
    string7 <>
    string8 <>
    string9 <>
    string10 <>
    string11 <>
    string12 <>
    string13 <>
    string14 <>
    string15 <>
    string16 <>
    string17 <>
    string18 <>
    string19 <>
    string20 <>
    string21 <>
    string22 <>
    string23 <>
    string24 <>
    string25 <>
    string26 <>
    string27 <>
    string28 <>
    string29
  where
    size = 1000
    rad = 50 :: Double
    left = size |* leftV

    pos :: Radius -> Vector -> Int -> Vector
    pos rad left i = left |+ ((fromIntegral i * rad * 0.9) |* rightV)
    chem :: Int -> Stripe
    chem i = case i `mod` 3 of
      0 -> No A
      1 -> No B
      2 -> No C
      _ -> error "Impossible. mod 3 is under 3"

    string0 = orbStruct $ Orb (pos rad left 0) (Yes A)
    string1 = orbStruct $ Orb (pos rad left 1) (chem 1)
    string2 = orbStruct $ Orb (pos rad left 2) (chem 2)
    string3 = orbStruct $ Orb (pos rad left 3) (chem 3)
    string4 = orbStruct $ Orb (pos rad left 4) (chem 4)
    string5 = orbStruct $ Orb (pos rad left 5) (chem 5)
    string6 = orbStruct $ Orb (pos rad left 6) (chem 6)
    string7 = orbStruct $ Orb (pos rad left 7) (chem 7)
    string8 = orbStruct $ Orb (pos rad left 8) (chem 8)
    string9 = orbStruct $ Orb (pos rad left 9) (chem 9)
    string10 = orbStruct $ Orb (pos rad left 10) (chem 10)
    string11 = orbStruct $ Orb (pos rad left 11) (chem 11)
    string12 = orbStruct $ Orb (pos rad left 12) (chem 12)
    string13 = orbStruct $ Orb (pos rad left 13) (chem 13)
    string14 = orbStruct $ Orb (pos rad left 14) (chem 14)
    string15 = orbStruct $ Orb (pos rad left 15) (chem 15)
    string16 = orbStruct $ Orb (pos rad left 16) (chem 16)
    string17 = orbStruct $ Orb (pos rad left 17) (chem 17)
    string18 = orbStruct $ Orb (pos rad left 18) (chem 18)
    string19 = orbStruct $ Orb (pos rad left 19) (chem 19)
    string20 = orbStruct $ Orb (pos rad left 20) (chem 20)
    string21 = orbStruct $ Orb (pos rad left 21) (chem 21)
    string22 = orbStruct $ Orb (pos rad left 22) (chem 22)
    string23 = orbStruct $ Orb (pos rad left 23) (chem 23)
    string24 = orbStruct $ Orb (pos rad left 24) (chem 24)
    string25 = orbStruct $ Orb (pos rad left 25) (chem 25)
    string26 = orbStruct $ Orb (pos rad left 26) (chem 26)
    string27 = orbStruct $ Orb (pos rad left 27) (chem 27)
    string28 = orbStruct $ Orb (pos rad left 28) (chem 28)
    string29 = orbStruct $ Orb (pos rad left 29) (chem 29)
