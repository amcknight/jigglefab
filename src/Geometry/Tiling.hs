module Geometry.Tiling
( Wedge(..)
, tileVoronoi
) where
import Geometry.Vector
import Geometry.Angle
import Geometry.Voronoi
import Geometry.Line
import qualified Data.Vector as V
import Orb

data Wedge = Tri Position Position Position Int | Pie Position Turn Turn Int deriving Show

tileVoronoi :: V.Vector (Orb c) -> [Edge] -> [Wedge]
tileVoronoi = concatMap . toWedges

toWedges :: V.Vector (Orb c) -> Edge -> [Wedge]
toWedges os (Edge (Seg a b) (i,j))
  | otherwise = [Tri ip a b i, Tri jp a b j]
  where
    ip = orbPos (os V.! i)
    jp = orbPos (os V.! j)