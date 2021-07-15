module Space
( Duration
, Radius
, Time
, Side (In, Out)
, Hit (Bounce, Pass)
, updateSide
, minFuture
) where

type Time = Float
type Duration = Float
type Radius = Float

data Side = In | Out deriving (Show, Eq)
data Hit = Bounce | Pass deriving Show

updateSide :: Side -> Hit -> Side
updateSide In  Bounce = In
updateSide In  Pass   = Out
updateSide Out Bounce = Out
updateSide Out Pass   = In

minFuture :: (Time, Time) -> Maybe Time
minFuture (t1, t2)
  | t1 < 0,  t2 < 0  = Nothing 
  | t1 >= 0, t2 < 0  = Just t1
  | t1 < 0,  t2 >= 0 = Just t2
  | t1 < t2          = Just t1
  | otherwise        = Just t2