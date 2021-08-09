module Ball
( Ball (Ball)
, point
, chem
, moveBall
) where

import Point
import Time

data Ball a = Ball
  { point :: Point
  , chem :: a
  } deriving Show

moveBall :: Duration -> Ball a -> Ball a
moveBall dt (Ball p c) = Ball (movePoint dt p) c
