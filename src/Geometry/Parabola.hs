module Geometry.Parabola
( Parabola(..)
, crossPointsFromFoci
, parabolaFromFocus
, atX
, parabPoss
) where
    
import Geometry.Vector
import Geometry.CrossPoint

data Parabola = Parabola
  { sharpness :: Double
  , downLeftness :: Double
  , upness :: Double
  } deriving (Show, Eq)

instance HasCrossPoints Parabola where
  crossPoints p1@(Parabola a1 b1 c1) (Parabola a2 b2 c2) =
    if a1 == a2
    then if b1 == b2
         then if c1 == c2
              then InfinteCross
              else NoCross
         else OneCross (atX p1 (-cob))
    else case compare discr 0 of
      LT -> NoCross
      EQ -> OneCross $ atX p1 $ -0.5 * boa
      GT -> TwoCross (atX p1 xLeft) (atX p1 xRight)
    where
      ad = a2 - a1
      bd = b2 - b1
      cd = c2 - c1
      boa = bd/ad
      coa = cd/ad
      cob = cd/bd
      discr = 0.25*boa*boa - coa
      xRight = sqrt discr - 0.5*boa
      xLeft = - sqrt discr - 0.5*boa

atX :: Parabola -> Double -> Position
atX (Parabola a b c) x = (x, a*x*x + b*x + c)

parabolaFromFocus :: Double -> Position -> Maybe Parabola
parabolaFromFocus sw (px,py)
  | py == sw = Nothing
  | otherwise = Just $ Parabola sh dl u
  where
    sh = 0.5 / (py - sw)
    dl = -2 * px * sh
    u = (px*px + py*py - sw*sw) * sh

crossPointsFromFoci :: Double -> Position -> Position -> CrossPoints
crossPointsFromFoci sw p1 p2
  | p1 == p2 = InfinteCross
  | otherwise = case (parabolaFromFocus sw p1, parabolaFromFocus sw p2) of
    (Nothing, Nothing) -> if fst p1 == fst p2 then InfinteCross else NoCross 
    (Nothing, Just p) -> OneCross $ atX p $ fst p1
    (Just p, Nothing) -> OneCross $ atX p $ fst p2
    (Just a, Just b) -> crossPoints a b

parabPoss :: Parabola -> (Double, Double) -> Double -> [Position]
parabPoss p (mnX,mxX) res = fmap (atX p) [mnX,(mnX+res)..mxX]
