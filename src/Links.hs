module Links
( Links
, points
, chems
, buildLinks
, asList
) where

import Points
import Chems
import Link
import Pair

type Links = (Link, Link)

points :: Links -> Points
points = bimap point

chems :: Links -> Chems
chems = bimap chem

buildLinks :: Points -> Chems -> Links
buildLinks (p1, p2) (c1, c2) = (Link p1 c1, Link p2 c2)

asList :: Links -> [Link]
asList (l1, l2) = [l1, l2]