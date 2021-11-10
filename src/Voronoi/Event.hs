module Voronoi.Event
( Cross(..)
, Bouy(..)
, Event(..)
, height
, isBouyEvent
) where
import Geometry.Vector
import Geometry.Circle

data Bouy = Bouy
  { bouyPos :: Position
  , bouyI :: Int
  } deriving Eq

instance Show Bouy where
  show (Bouy pos i) = "Bouy "++show pos++" i"++show i

instance HasPos Bouy where
  pos = bouyPos

data Cross = Cross
  { crossC :: Circle
  , crossI :: Int
  } deriving Eq

instance HasPos Cross where
  pos = pos . crossC

instance Show Cross where
  show (Cross c i) = "Cross "++show c++" i"++show i

data Event = BouyEvent Bouy | CrossEvent Cross deriving Eq

instance Show Event where
  show (BouyEvent e) = "Event"++show e
  show (CrossEvent e) = "Event"++show e

height :: Event -> Double
height (BouyEvent (Bouy (_,y) _)) = y
height (CrossEvent (Cross (Circle (_,y) r) _)) = y - r

instance Ord Event where
  compare e1 e2 = compare (height e2) (height e1)

isBouyEvent :: Event -> Bool
isBouyEvent (BouyEvent _) = True
isBouyEvent _ = False
