module Link
( Link (Link)
, point
, chem
, moveLink
) where

import Point
import Chem
import Time

data Link = Link
  { point :: Point
  , chem :: Chem
  } deriving Show

moveLink :: Duration -> Link -> Link
moveLink dt (Link p c) = Link (movePoint dt p) c
