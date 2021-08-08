module Ball
( Ball (Ball)
, point
, chem
, moveBall
) where

import Point
import Chem
import Time

data Ball = Ball
  { point :: Point
  , chem :: Chem
  } deriving Show

moveBall :: Duration -> Ball -> Ball
moveBall dt (Ball p c) = Ball (movePoint dt p) c
