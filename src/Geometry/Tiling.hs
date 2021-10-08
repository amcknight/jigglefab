module Geometry.Tiling
( Wedge(..)
, tileVoronoi
) where
import Geometry.Vector
import Geometry.Angle
import Geometry.Voronoi
import Geometry.Line

data Wedge = Tri Position Position Position Int | Pie Position Turn Turn Int deriving Show

tileVoronoi :: [Edge] -> [Wedge]
tileVoronoi = concatMap toWedges

--TODO: THis is wrong
toWedges :: Edge -> [Wedge]
toWedges (Edge (Seg p q) (i,j)) = [Tri zeroV p q i, Tri zeroV p q j]
