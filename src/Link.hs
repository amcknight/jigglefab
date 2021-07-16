module Link
( Link (Link)
, point
, chem
, moveLink
) where

import Point
import Chem
import Space

data Link = Link Point Chem deriving Show

point :: Link -> Point
point (Link p _) = p

chem :: Link -> Chem
chem (Link _ c) = c

moveLink :: Duration -> Link -> Link
moveLink dt (Link p c) = Link (movePoint dt p) c
