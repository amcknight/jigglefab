module Tiling
( Wedge(..)
, tileVoronoi
, buildTri
) where

import Data.List (sortBy, sortOn, groupBy)
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Geometry.Vector
import Geometry.Line
import Pair
import Geometry.CrossPoint
import Voronoi.Pie
import Voronoi.Tri
import Voronoi.Sweep
import Voronoi.Edge

data Wedge = TriWedge Int Tri | PieWedge Int Pie deriving Show

wedgeI :: Wedge -> Int
wedgeI (TriWedge i _) = i
wedgeI (PieWedge i _) = i

tileVoronoi :: V.Vector Position -> [Edge] -> [Wedge]
tileVoronoi ps es = ts ++ toPies ps ts
  where ts = concatMap (toTris ps) es

toPies :: V.Vector Position -> [Wedge] -> [Wedge]
toPies ps = concat . toPies' (V.toList (V.indexed ps)) . groupBy (\t1 t2 -> wedgeI t1 == wedgeI t2) . sortOn wedgeI

toPies' :: [(Int, Position)] -> [[Wedge]] -> [[Wedge]]
toPies' [] [] = []
toPies' [] _ = error "Out of Orbs but still have Tris with Orb indices"
toPies' ((i,p):ips) [] = extractPie i p [] : toPies' ips []
toPies' _ ([]:_) = error "Somehow we have an empty Tri list"
toPies' ((i,p):ips) (ts:tss) = case compare i (wedgeI (head ts)) of
   LT -> extractPie i p [] : toPies' ips (ts:tss)
   EQ -> extractPie i p ts : toPies' ips tss
   GT -> error "Orb index should never be above Tri index"

extractPie :: Int -> Position -> [Wedge] -> [Wedge]
extractPie i p [] = [PieWedge i (Pie p FullSweep)]
extractPie i _ ts = fmap (PieWedge i) $ antiPies $ fmap (triToPie . (\(TriWedge _ t) -> t)) ts

antiPies :: [Pie] -> [Pie]
antiPies ps = fmap (Pie (pos (head ps))) . antiSweeps . sortOn (\(Sweep t _) -> t) . fmap (\(Pie _ s) -> s) $ ps

triToPie :: Tri -> Pie
triToPie (Tri o (Seg p q)) = case (direction (p |- o), direction (q |- o)) of
  (Nothing,Nothing) -> error "Degenerate Tri can't be converted to Pie"
  (Nothing, _) -> error "Degenerate Tri gives empty Pie but this also shouldn't happen. p == o"
  (_, Nothing) -> error "Degenerate Tri gives empty Pie but this also shouldn't happen. q == o"
  (Just dpo, Just dqo) -> Pie o $ Sweep dpo dqo

toTris :: V.Vector Position -> Edge -> [Wedge]
toTris ps (Edge (Seg p1 p2) is@(i,j)) = catMaybes
  [ fmap (TriWedge i) (buildTri p1 p2 cps o1)
  , fmap (TriWedge j) (buildTri p1 p2 cps o2)
  ]
  where
    (o1, o2) = pmap (ps V.!) is
    o = o1 -- o1 or o2 would both give same result here
    cps = case crossPointsAtUnit (Line (p1 |- o) (p2 |- o)) of
      OneCross c -> OneCross (c |+ o)
      TwoCross c1 c2 -> TwoCross (c1 |+ o) (c2 |+ o)
      a -> a

buildTri :: Position -> Position -> CrossPoints -> Position -> Maybe Tri
buildTri _ _ InfinteCross _ = error "Impossible for a circle and line to share more than two points"
buildTri p1 p2 (TwoCross c1 c2) o = case (compare (distSq p1 o) 1, compare (distSq p2 o) 1) of
  (GT, GT) -> if turnDirection o c1 p1 == turnDirection o c1 p2
    then Nothing -- Both outside but don't cross
    else let (s1, s2, _) = sort3By p1 (p2,c1,c2) in Just $ makeCCW $ Tri o $ Seg s1 s2 -- Both outside but cross
  (GT,  _) -> let (s1, s2, _) = sort3By p1 (p2,c1,c2) in Just $ makeCCW $ Tri o $ Seg s1 s2 -- Left out right in
  ( _, GT) -> let (s1, s2, _) = sort3By p2 (p1,c1,c2) in Just $ makeCCW $ Tri o $ Seg s1 s2 -- Right out left in
  ( _,  _) -> Just $ makeCCW $ Tri o $ Seg p1 p2 -- Both in
buildTri _ _ _ _ = Nothing

sort3By :: Position -> (Position, Position, Position) -> (Position, Position, Position)
sort3By out (a,b,c) = (d,e,f)
  where [d, e, f] = sortBy (\p q -> compare (distSq p out) (distSq q out)) [a,b,c] 
