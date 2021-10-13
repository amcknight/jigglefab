module Geometry.Parabola
( Parabola(..)
, crossPointsFromFocus
) where
    
import Geometry.Vector
import Geometry.CrossPoint

data Parabola = Parabola
  { sharpness :: Float
  , downLeftness :: Float
  , upness :: Float
  } deriving (Show, Eq)

instance HasCrossPoints Parabola where
  crossPoints p1@(Parabola a1 b1 c1) p2@(Parabola a2 b2 c2) =
    if a1 == a2
    then if b1 == b2
         then if c1 == c2
              then AllCross
              else NoCross
         else OneCross (atX p1 (-cob))
    else case compare discr 0 of
      LT -> NoCross
      EQ -> OneCross undefined
      GT -> TwoCross (atX p1 xLeft) (atX p1 xRight)
    where
      ad = a2 - a1
      bd = b2 - b1
      cd = c2 - c1
      boa = bd/ad
      coa = cd/ad
      cob = cd/bd
      discr = boa^2 - coa
      xRight = sqrt discr - 0.5*boa
      xLeft = - sqrt discr - 0.5*boa

atX :: Parabola -> Float -> Position
atX (Parabola a b c) x = (x, a*x^2 + b*x + c)

atY :: Parabola -> Float -> CrossPoints
atY = undefined

parabolaFromFocus :: Float -> Position -> Parabola
parabolaFromFocus sw (px,py) = Parabola sh dl u
  where
    sh = 0.5/(py - sw)
    dl = -2 * px * sh
    u = (px^2 + py^2 - sw^2) * sh

crossPointsFromFocus :: Float -> Position -> Position -> CrossPoints
crossPointsFromFocus sw p1 p2 = crossPoints (parabolaFromFocus sw p1) (parabolaFromFocus sw p2)
