module Links
( Links
, points
, chems
, links
, contact
) where

import Vectors -- Probably shouldn't need to minus Vectors in Links
import Point
import Points
import Chems
import Space
import Link

type Links = (Link, Link)

points :: Links -> Points
points (l1, l2) = (point l1, point l2)

chems :: Links -> Chems
chems (l1, l2) = (chem l1, chem l2)

links :: Points -> Chems -> Links
links (p1, p2) (c1, c2) = (Link p1 c1, Link p2 c2)

earliestHitTime :: Radius -> Link -> Link -> Duration
earliestHitTime rad l1 l2 = max xt yt
  where
    xt = (dx - rad) / dvx
    yt = (dy - rad) / dvy
    (dx, dy)   = pos (point l2) |- pos (point l1)
    (dvx, dvy) = vel (point l1) |- vel (point l2)

contact :: Side -> Links -> Links
contact s ls = links newPs newCs
  where
    (Products hit newCs) = react $ Reactants s $ chems ls
    newPs = case hit of 
      Pass -> points ls
      Bounce -> bounce $ points ls