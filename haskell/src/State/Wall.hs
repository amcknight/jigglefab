module State.Wall
( Wall (..)
, rock
, wSide
) where

import Motion.Point
import Geometry.Vector
import Geometry.Circle
import GHC.Generics (Generic)
import Data.Serialize (Serialize)
import Util.Side

data Wall = VLine Double | HLine Double | Rock Circle deriving (Eq, Show, Generic)

instance Serialize Wall

rock :: Position -> Radius -> Wall
rock p = Rock . Circle p

wSide :: Wall -> Position -> Side
wSide (VLine x) p = wSide' x $ fst p
wSide (HLine y) p = wSide' y $ snd p
wSide (Rock (Circle c r)) p = if distSq p c > r*r then Out else In
wSide' :: Double -> Double -> Side
wSide' pl p = case compare pl p of
  LT -> In
  _ -> Out
