module Geometry.Tiling
( Wedge(..)
, tileVoronoi
) where
import Geometry.Vector
import Geometry.Voronoi
import Geometry.Line
import qualified Data.Vector as V
import Orb
import Color
import Chem
import Data.List (sortBy)
import Geometry.Angle
import Pair
import Debug.Trace
import Geometry.CrossPoint

data Wedge = Tri Position Position Position Color | Pie Position Turn Turn Color deriving Show

tileVoronoi :: Chem c => V.Vector (Orb c) -> [Edge] -> [Wedge]
tileVoronoi = concatMap . toWedges

toWedges :: Chem c => V.Vector (Orb c) -> Edge -> [Wedge]
toWedges os (Edge s@(Seg p1 p2) is) = buildWedge p1 p2 cps o1 ++ buildWedge p1 p2 cps o2
  where
    cps = case crossPointsAtUnit (Line (p1 |- origOrbPos) (p2 |- origOrbPos)) of
      OneCross c -> OneCross (c |+ origOrbPos)
      TwoCross c1 c2 -> TwoCross (c1 |+ origOrbPos) (c2 |+ origOrbPos)
      a -> a
    origOrbPos = orbPos o1
    (o1, o2) = pmap (os V.!) is

buildWedge :: Chem c => Position -> Position -> CrossPoints -> Orb c -> [Wedge]
buildWedge p1 p2 NoCross (Orb o ch) = [toPie o p1 p2 (chemColor ch)]
buildWedge p1 p2 (OneCross c) (Orb o ch) = undefined 
buildWedge p1 p2 (TwoCross c1 c2) (Orb o ch) = case (compare (distSq p1 o) 1, compare (distSq p2 o) 1) of
  (GT, GT) -> -- Both outside but cross"
    let (s1, s2, s3) = sort4 p1 (p2,c1,c2)
    in [toPie o p1 s1 c, Tri o s1 s2 c, toPie o s2 s3 c]
  (GT, _) -> -- Left out right in" at
    let (s1, s2, s3) = sort4 p1 (p2,c1,c2)
    in [toPie o p1 s1 c, Tri o s1 s2 c]
  (_, GT) -> -- Right out left in" ta
    let (s1, s2, s3) = sort4 p2 (p1,c1,c2)
    in [toPie o p2 s1 c, Tri o s1 s2 c]
  (_, _) -> [Tri o p1 p2 c] -- Both in
  where
    c = chemColor ch
buildWedge _ _ AllCross _ = error "Impossible for a circle and line to share all points"

sort4 :: Position -> (Position, Position, Position) -> (Position, Position, Position)
sort4 out (a,b,c) = (d,e,f)
  where [d, e, f] = sortBy (\p q -> compare (distSq p out) (distSq q out)) [a,b,c] 

toPie :: Position -> Position -> Position -> Color -> Wedge
toPie o p1 p2 = Pie o (direction (o |- p1)) (direction (o |- p2))
