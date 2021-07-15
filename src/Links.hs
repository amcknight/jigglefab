module Links
( Links
, points
, chems
, buildLinks
) where

import Points
import Chems
import Link

type Links = (Link, Link)

points :: Links -> Points
points (l1, l2) = (point l1, point l2)

chems :: Links -> Chems
chems (l1, l2) = (chem l1, chem l2)

buildLinks :: Points -> Chems -> Links
buildLinks (p1, p2) (c1, c2) = (Link p1 c1, Link p2 c2)
