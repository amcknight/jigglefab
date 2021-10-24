module Geometry.Tiling
( Wedge(..)
, tileVoronoi
, antiSweeps
) where

import Data.List (sortBy, sortOn, groupBy)
import Data.Maybe (catMaybes)
import Geometry.Vector
import Geometry.Voronoi
import Geometry.Line
import qualified Data.Vector as V
import Orb
import Color
import Chem
import Geometry.Angle
import Pair
import Debug.Trace
import Geometry.CrossPoint
import Geometry.Pie
import Geometry.Tri
import Geometry.Sweep

data Wedge = TriWedge Int Tri | PieWedge Int Pie deriving Show

wedgeI :: Wedge -> Int
wedgeI (TriWedge i _) = i
wedgeI (PieWedge i _) = i

tileVoronoi :: Chem c => V.Vector (Orb c) -> [Edge] -> [Wedge]
tileVoronoi os es = ts ++ toPies os ts
  where ts = concatMap (toTris os) es

toPies :: V.Vector (Orb c) -> [Wedge] -> [Wedge]
toPies os = concat . toPies' (V.toList (V.indexed os)) . groupBy (\t1 t2 -> wedgeI t1 == wedgeI t2) . sortOn wedgeI

toPies' :: [(Int, Orb c)] -> [[Wedge]] -> [[Wedge]]
toPies' [] [] = []
toPies' [] _ = error "Out of Orbs but still have Tris with Orb indices"
toPies' ((i,o):ios) [] = extractPie i (orbPos o) [] : toPies' ios []
toPies' _ ([]:_) = error "Somehow we have an empty Tri list"
toPies' ((i,o):ios) (ts:tss) = extractPie i (orbPos o) (if i == wedgeI (head ts) then ts else []) : toPies' ios tss

extractPie :: Int -> Position -> [Wedge] -> [Wedge]
extractPie i p [] = [PieWedge i (Pie p FullSweep)]
extractPie i p ts = fmap (PieWedge i) $ antiPies $ fmap (triToPie . (\(TriWedge i t) -> t)) ts

antiPies :: [Pie] -> [Pie]
antiPies ps = fmap (Pie o) . antiSweeps . sortOn (\(Sweep t _) -> t) . fmap (\(Pie _ s) -> s) $ ps
  where o = piePos $ head ps

triToPie :: Tri -> Pie
triToPie (Tri o (Seg p q)) = Pie o $ Sweep (direction (p |- o)) (direction (q |- o))

toTris :: Chem c => V.Vector (Orb c) -> Edge -> [Wedge]
toTris os (Edge s@(Seg p1 p2) is@(i,j)) = catMaybes
  [ fmap (TriWedge i) (buildTri p1 p2 cps o1)
  , fmap (TriWedge j) (buildTri p1 p2 cps o2)
  ]
  where
    -- o1 or o2 would both give same result here
    cps = case crossPointsAtUnit (Line (p1 |- o1) (p2 |- o1)) of
      OneCross c -> OneCross (c |+ o1)
      TwoCross c1 c2 -> TwoCross (c1 |+ o1) (c2 |+ o1)
      a -> a
    (o1, o2) = pmap (orbPos . (os V.!)) is

buildTri :: Position -> Position -> CrossPoints -> Position -> Maybe Tri
buildTri p1 p2 (TwoCross c1 c2) o = Just $ makeCCW $ Tri o $ Seg x1 x2
  where
    (x1, x2) = case (compare (distSq p1 o) 1, compare (distSq p2 o) 1) of
      (GT, GT) -> let (s1, s2, _) = sort4 p1 (p2,c1,c2) in (s1, s2) -- Both outside but cross
      (GT,  _) -> let (s1, s2, _) = sort4 p1 (p2,c1,c2) in (s1, s2) -- Left out right in
      ( _, GT) -> let (s1, s2, _) = sort4 p2 (p1,c1,c2) in (s1, s2) -- Right out left in
      ( _,  _) -> (p1, p2) -- Both in
buildTri _ _ AllCross _ = error "Impossible for a circle and line to share all points"
buildTri _ _ _ _ = Nothing

sort4 :: Position -> (Position, Position, Position) -> (Position, Position, Position)
sort4 out (a,b,c) = (d,e,f)
  where [d, e, f] = sortBy (\p q -> compare (distSq p out) (distSq q out)) [a,b,c] 
