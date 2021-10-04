module Geometry.Tiling
  ( Voronoi
  , voronoi
  ) where
import Graphics.Gloss (Picture)
import Geometry.Vector
import Geometry.Angle
import Data.List (sortBy)

type IxPos = (Position, Int)

data Arc = Arc
  { cenPos :: Position
  , from :: Turn
  , to
  , arcI :: Int
  }
data Tri = Tri
  { trip :: (Position, Position, Position)
  , triI :: Int 
  }
data Bouy = Bouy
  { pos :: Position
  , leftPos :: Position
  , rightPos :: Position
  , bouyI :: Int
  }

data Voronoi = Voronoi
  { arcs :: [Arc]
  , tris :: [Tri]
  }

data Beach = Beach
  { verts :: [IxPos] -- Chem index (Assumes sorted by Y)
  , circs :: [IxPos] -- Bouy Index
  , bouys :: [Bouy]
  , voron :: Voronoi
  }

voronoi :: [IxPos] -> Voronoi
voronoi vs = voronoi' $ Beach vs [] [] (Voronoi [] [])
  where
    maxY = fst $ head vs
    voronoi' :: Beach -> Voronoi
    voronoi' b = case b of
      (Beach [] [] _ v) -> v
      _ -> voronoi' $ moveBeach b

moveBeach :: Beach -> Beach
moveBeach b@(Beach [] [] _ v) = b -- shouldn't happen
moveBeach b@(Beach [] (c:cs) _ _) = removeBouy c $ b { circs = cs }
moveBeach b@(Beach (p:ps) [] _ _) = addBouy p $ b { verts = ps }
moveBeach b@(Beach (p@((_,py),_):ps) (c@((_,cy),_):cs) _ _) = case compare py cy of
  LT -> addBouy p $ b { verts = ps }
  _ -> removeBouy c $ b { circs = cs }

removeBouy :: IxPos -> Beach -> Beach
removeBouy c (Beach ps cs bs v) = case bs of
  [] -> undefined
  [_] -> undefined
  [_,_] -> undefined
  bs -> case (bs, c) of
    (_, (_,0)) -> undefined 
    ([_,_,_], (_,2)) -> undefined
    (lb:(b:(rb:bs)), (p,1)) -> let
      newTris =
        [ Tri (pos lb, rightPos lb, p) (bouyI lb)
        , Tri (pos b, leftPos b, p) (bouyI b)
        , Tri (pos b, rightPos b, p) (bouyI b)
        , Tri (pos rb, leftPos rb, p) (bouyI rb)
        ]
      in Beach ps cs (lb{ rightPos = p } : rb{ leftPos = p } : bs) (v{ tris = tris v ++ newTris })
    (bs, (p,i)) -> let
      newB = removeBouy c $ Beach ps cs (drop (i-1) bs) v
      in newB { bouys = take (i-1) bs ++ bouys newB }

addBouy :: IxPos -> Beach -> Beach
addBouy p (Beach ps cs bs v) = undefined -- TODO
